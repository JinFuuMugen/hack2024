module liteic_slave_node_write
    import liteic_pkg::IC_NUM_MASTER_SLOTS;
    import liteic_pkg::IC_AWADDR_WIDTH;
    import liteic_pkg::IC_WDATA_WIDTH;
    import liteic_pkg::IC_BRESP_WIDTH;
    import liteic_pkg::IC_WR_CONNECTIVITY;
(
    input logic clk_i,
    input logic rstn_i,

    // interconnect i/o
    axi_lite_if slv_axil,

    // node matrix i/o
    input  logic [ IC_WDATA_WIDTH-1      : 0 ] cbar_w_reqst_data_i  [ IC_NUM_MASTER_SLOTS ],
    input  logic [ 3                     : 0 ] cbar_reqst_awqos_i [ IC_NUM_MASTER_SLOTS ],
    input  logic [ IC_NUM_MASTER_SLOTS-1 : 0 ] cbar_w_reqst_val_i,
    output logic [ IC_NUM_MASTER_SLOTS-1 : 0 ] cbar_w_reqst_rdy_o,

    input  logic [ IC_AWADDR_WIDTH-1     : 0 ] cbar_aw_reqst_data_i  [ IC_NUM_MASTER_SLOTS ],
    input  logic [ IC_NUM_MASTER_SLOTS-1 : 0 ] cbar_aw_reqst_val_i,
    output logic [ IC_NUM_MASTER_SLOTS-1 : 0 ] cbar_aw_reqst_rdy_o,

    input  logic [ IC_NUM_MASTER_SLOTS-1 : 0 ] cbar_resp_rdy_i,
    output logic [ IC_NUM_MASTER_SLOTS-1 : 0 ] cbar_resp_val_o,
    output logic [ IC_BRESP_WIDTH-1      : 0 ] cbar_resp_data_o
);


//-------------------------------------------------------------------------------
// localparams
//-------------------------------------------------------------------------------

// Find count of already connected master slots in interconnect
function int unsigned count_master_slots();
    count_master_slots = 0;
    for(int i = 0; i < IC_NUM_MASTER_SLOTS; i++) begin
        if(IC_WR_CONNECTIVITY[i])
            count_master_slots++;
    end
endfunction

// determine node's master slots number
localparam NODE_NUM_MASTER_SLOTS   = count_master_slots();

// get n-th non-zero position in connectivity vector
function int unsigned get_connectivity_idx(int n);
    int connectivity_idx;
    connectivity_idx = 0;
    for(int i = 0; i < IC_NUM_MASTER_SLOTS; i++) begin
        if(IC_WR_CONNECTIVITY[i]) begin
            if (connectivity_idx == n)
                return i;
            else
                connectivity_idx++;
        end
    end
endfunction

localparam NODE_MASTER_ID_WIDTH   = $clog2(NODE_NUM_MASTER_SLOTS);

//-------------------------------------------------------------------------------
// signals & interfaces
//-------------------------------------------------------------------------------

// Handling node's connectivity to interconnect crossbar matrix

logic [ IC_WDATA_WIDTH-1        : 0 ] node_wdata_w              [ NODE_NUM_MASTER_SLOTS ] ;
logic [ NODE_NUM_MASTER_SLOTS-1 : 0 ] node_wvalid_w;
logic [ NODE_NUM_MASTER_SLOTS-1 : 0 ] node_wready_w;
logic [ IC_AWADDR_WIDTH-1       : 0 ] node_awaddr               [ NODE_NUM_MASTER_SLOTS ] ;
logic [ NODE_NUM_MASTER_SLOTS-1 : 0 ] node_awvalid_w;
logic [ NODE_NUM_MASTER_SLOTS-1 : 0 ] node_awready_w;
logic [ NODE_NUM_MASTER_SLOTS-1 : 0 ] node_bready_w;
logic [ NODE_NUM_MASTER_SLOTS-1 : 0 ] node_bvalid_w;
logic [ IC_BRESP_WIDTH-1        : 0 ] node_bresp_w;


logic [3:0]                          node_awqos_w [  NODE_NUM_MASTER_SLOTS ];

// IDs of masters, which sent requests
logic [ NODE_NUM_MASTER_SLOTS-1:0 ] mst_id_reqst_onehot;
logic [ NODE_NUM_MASTER_SLOTS-1:0 ] mst_id_reqst_prior_onehot;
logic [ NODE_NUM_MASTER_SLOTS-1:0 ] mst_id_reqst_prior_onehot_r;

logic [ NODE_MASTER_ID_WIDTH-1 :0 ] mst_id_reqst;
logic [ NODE_MASTER_ID_WIDTH-1 :0 ] mst_id_reqst_prior;
logic [ NODE_MASTER_ID_WIDTH-1 :0 ] mst_id_reqst_prior_r;

// Signals from/to AXI slave
logic [ IC_WDATA_WIDTH-1 : 0   ] slv_wdata_wo;
logic                            slv_wvalid_wo;
logic                            slv_wready_wi;

logic [ IC_AWADDR_WIDTH-1 : 0  ] slv_awaddr_wo;
logic                            slv_awvalid_wo;
logic                            slv_awready_wi;

logic [ IC_BRESP_WIDTH-1   : 0 ] slv_bresp_wi;
logic                            slv_bvalid_wi;
logic                            slv_bready_wo;

// Flags
logic aw_success;
logic w_success;
logic aw_success_r;
logic w_success_r;
logic node_busy;

//-------------------------------------------------------------------------------
// = Reconnect and combine interfaces
//-------------------------------------------------------------------------------

assign slv_wready_wi                      = slv_axil.w_ready;
assign slv_axil.w_valid                   = slv_wvalid_wo;
assign {slv_axil.w_strb, slv_axil.w_data} = slv_wdata_wo;

assign slv_axil.aw_addr                   = slv_awaddr_wo;
assign slv_axil.aw_valid                  = slv_awvalid_wo ;
assign slv_awready_wi                     = slv_axil.aw_ready;

assign slv_bresp_wi                       = slv_axil.b_resp;
assign slv_bvalid_wi                      = slv_axil.b_valid;
assign slv_axil.b_ready                   = slv_bready_wo;

//-------------------------------------------------------------------------------
// Reconnect crossbar, if nodes has no connection
//-------------------------------------------------------------------------------

assign cbar_resp_data_o = node_bresp_w;
generate
    for (genvar node_mst_slot_idx = 0; node_mst_slot_idx < NODE_NUM_MASTER_SLOTS; node_mst_slot_idx++) begin
        localparam ic_mst_slot_idx = get_connectivity_idx(node_mst_slot_idx);

        assign node_awqos_w[node_mst_slot_idx]      = cbar_reqst_awqos_i[ic_mst_slot_idx];
        assign node_wdata_w[node_mst_slot_idx]      = cbar_w_reqst_data_i[ic_mst_slot_idx];
        assign node_wvalid_w [node_mst_slot_idx]    = cbar_w_reqst_val_i [ic_mst_slot_idx];
        assign cbar_w_reqst_rdy_o[ic_mst_slot_idx]  = node_wready_w[node_mst_slot_idx];

        assign node_awaddr[node_mst_slot_idx]       = cbar_aw_reqst_data_i[ic_mst_slot_idx];
        assign node_awvalid_w [node_mst_slot_idx]   = cbar_aw_reqst_val_i [ic_mst_slot_idx];
        assign cbar_aw_reqst_rdy_o[ic_mst_slot_idx] = node_awready_w[node_mst_slot_idx];

        assign node_bready_w  [node_mst_slot_idx]   = cbar_resp_rdy_i  [ic_mst_slot_idx];
        assign cbar_resp_val_o [ic_mst_slot_idx]    = node_bvalid_w [node_mst_slot_idx];
    end
endgenerate

//-------------------------------------------------------------------------------
// AXI signal management
//-------------------------------------------------------------------------------

// Define mst id, from which the request came
assign mst_id_reqst        = (!node_busy) ? mst_id_reqst_prior : mst_id_reqst_prior_r;
// The same, but onehot
assign mst_id_reqst_onehot = (!node_busy) ? mst_id_reqst_prior_onehot : mst_id_reqst_prior_onehot_r;

// = AW channel = //
 
assign slv_awvalid_wo   = (((node_awvalid_w)) && (!aw_success_r));
assign node_awready_w = (slv_awready_wi && (!aw_success_r)) ? mst_id_reqst_onehot : '0;
assign slv_awaddr_wo    = node_awaddr[mst_id_reqst];

// = W channel = //

assign slv_wvalid_wo  = ((mst_id_reqst_onehot & node_wvalid_w)) && (!w_success_r);
assign node_wready_w  = (slv_wready_wi && (!w_success_r)) ? mst_id_reqst_onehot : '0;
assign slv_wdata_wo   = node_wdata_w[mst_id_reqst];

// = B channel = //

assign node_bvalid_w = (slv_bvalid_wi) ? mst_id_reqst_prior_onehot_r : '0;
//assign slv_bready_wo = |(mst_id_reqst_prior_onehot_r & node_bready_w);
assign slv_bready_wo = (mst_id_reqst_prior_onehot_r & node_bready_w)? 1:0;
assign node_bresp_w  = slv_bresp_wi;

//-------------------------------------------------------------------------------
// Save id of master, which sent the reqst
//-------------------------------------------------------------------------------

always_ff @(posedge clk_i or negedge rstn_i)
if      (!rstn_i)                      mst_id_reqst_prior_onehot_r <= '0;
else if (slv_awvalid_wo && !node_busy) mst_id_reqst_prior_onehot_r <= mst_id_reqst_prior_onehot;
else                                   mst_id_reqst_prior_onehot_r <= mst_id_reqst_prior_onehot_r;

always_ff @(posedge clk_i or negedge rstn_i)
if      (!rstn_i)                      mst_id_reqst_prior_r <= '0;
else if (slv_awvalid_wo && !node_busy) mst_id_reqst_prior_r <= mst_id_reqst_prior;
else                                   mst_id_reqst_prior_r <= mst_id_reqst_prior_r;

//-------------------------------------------------------------------------------
// Flags of busy node
//-------------------------------------------------------------------------------

always_ff @(posedge clk_i or negedge rstn_i)
if      (!rstn_i)                       node_busy <= 'b0;
else if (slv_bvalid_wi & slv_bready_wo) node_busy <= 'b0;
//else if (|node_awvalid_w              ) node_busy <= 'b1;
else if (node_awvalid_w              ) node_busy <= 'b1;
else                                    node_busy <= node_busy;

//-------------------------------------------------------------------------------
// Flags of success transactions
//-------------------------------------------------------------------------------

assign aw_success   = slv_awvalid_wo && slv_awready_wi;
always_ff @(posedge clk_i or negedge rstn_i)
if      (!rstn_i)                       aw_success_r <= 'b0;
else if (slv_bvalid_wi & slv_bready_wo) aw_success_r <= 'b0;
else                                    aw_success_r <= aw_success_r | aw_success;

assign  w_success   =  slv_wvalid_wo &&  slv_wready_wi;
always_ff @(posedge clk_i or negedge rstn_i)
if      (!rstn_i)                       w_success_r <= 'b0;
else if (slv_bvalid_wi & slv_bready_wo) w_success_r <= 'b0;
else                                    w_success_r <= w_success_r | w_success;

//...............................................................................
//  CheckPriority
//...............................................................................
//always_ff @(posedge clk_i or negedge rstn_i) begin
//    if(!rstn_i) begin
//        max_index = 0;
//        ind = 0;
//    end else begin
//        if(ind != max_index) begin
//            if((node_arqos_w[ind] > node_arqos_w[max_index]  || cbar_reqst_val_i[max_index] == 0) && cbar_reqst_val_i[ind] == 1) begin
//                max_index = ind;
//            end
//            ind <= (ind + 1)% 20;
//        end else begin
//            ind <= (ind + 1)% 20;
//        end
//    end
//end

logic [15:0]                          node_awqos_shift [  NODE_NUM_MASTER_SLOTS ];

for (genvar j = 0; j < NODE_NUM_MASTER_SLOTS; j = j + 1) begin 
    assign node_awqos_shift[j] = cbar_aw_reqst_val_i[j] << cbar_reqst_awqos_i[j];
end

logic [20-1:0] mask [5-1:0];
logic [15 : 0] qos_sum;
logic [15 : 0] tmp_or;
logic tmp_or_bit;

always_comb begin
    for (int i = 0; i < 16; i++) begin
        tmp_or_bit = 1'b0;
        for (int j = 0; j < NODE_NUM_MASTER_SLOTS; j++) begin
            tmp_or_bit = tmp_or_bit | node_awqos_shift[j][i];
        end
        tmp_or[i] = |tmp_or_bit;
    end
end


//for (genvar i = 0; i < 32; i = i + 1) begin
//    for(genvar j = 0; j < NODE_NUM_MASTER_SLOTS; j = j + 1) begin
//        assign tmp_or_bit[j][i] = node_arqos_shift[j][i];
//    end
//    assign tmp_or[i] = |tmp_or_bit[i];
//end

for (genvar i = 0; i < 5; i = i + 1) begin
    for(genvar j = 0; j < 20; j = j + 1) begin
        assign mask[i][j] = (j >> i) & 1;
    end
end

  
assign qos_sum =  tmp_or;


logic [31:0] one_shot_catch;

always_comb begin
    for(int i = 0; i < NODE_NUM_MASTER_SLOTS; i = i + 1) begin    
        if(|cbar_aw_reqst_val_i) begin
            if(node_awqos_shift[i] == one_shot_catch) begin
                mst_id_reqst_prior_onehot = (1<<i);
            end
        end else begin
            mst_id_reqst_prior_onehot = '0;
        end
    end
end

for(genvar i = 0; i < 5; i = i + 1) begin    
    assign mst_id_reqst_prior[i] = |(mst_id_reqst_prior_onehot & mask[i]);
end

//-------------------------------------------------------------------------------
// initializations units
//-------------------------------------------------------------------------------

liteic_priority_cd #(.IN_WIDTH(16), .OUT_WIDTH(16), .IC_NUM_MASTER_SLOTS(IC_NUM_MASTER_SLOTS))  
master_aw_reqst_priority_cd (
    .in     (qos_sum            ) ,
    .onehot (one_shot_catch ) //,
   // .out    (mst_id_reqst_prior        )
);

endmodule