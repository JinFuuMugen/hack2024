module liteic_slave_node_read
    import liteic_pkg::IC_NUM_MASTER_SLOTS;
    import liteic_pkg::IC_ARADDR_WIDTH;
    import liteic_pkg::IC_RDATA_WIDTH;
    import liteic_pkg::IC_RD_CONNECTIVITY;

(
    input logic                                    clk_i,
    input logic                                    rstn_i,
    
    // interconnect i/o
    axi_lite_if                                    slv_axil,

    // node matrix i/o
    input  logic [ IC_ARADDR_WIDTH-1     : 0 ] cbar_reqst_data_i  [ IC_NUM_MASTER_SLOTS ],
    input  logic [ 3                     : 0 ] cbar_reqst_arqos_i [ IC_NUM_MASTER_SLOTS ],
    input  logic [ IC_NUM_MASTER_SLOTS-1 : 0 ] cbar_reqst_val_i,
    output logic [ IC_NUM_MASTER_SLOTS-1 : 0 ] cbar_reqst_rdy_o,

    input  logic [ IC_NUM_MASTER_SLOTS-1 : 0 ] cbar_resp_rdy_i,
    output logic [ IC_NUM_MASTER_SLOTS-1 : 0 ] cbar_resp_val_o,
    output logic [ IC_RDATA_WIDTH-1      : 0 ] cbar_resp_data_o,
    output logic [ 4                     : 0 ] max_index_o,
    output logic [ 4                     : 0 ] ind_o
);

logic [4 : 0] max_index;
logic [4 : 0] ind;

assign max_index_o = max_index;
assign ind_o       = ind;
//-------------------------------------------------------------------------------
// localparams
//-------------------------------------------------------------------------------

function int unsigned count_master_slots();
    count_master_slots = 0;
    for(int i = 0; i < IC_NUM_MASTER_SLOTS; i++) begin
        if(IC_RD_CONNECTIVITY[i])
            count_master_slots++;
    end
endfunction

// get n-th non-zero position in connectivity vector
function int unsigned get_connectivity_idx(int n);
    int connectivity_idx;
    connectivity_idx = 0;
    for(int i = 0; i < IC_NUM_MASTER_SLOTS; i++) begin
        if(IC_RD_CONNECTIVITY[i]) begin
            if (connectivity_idx == n)
                return i;
            else
                connectivity_idx++;
        end
    end
endfunction

// determine node's master slots number
localparam NODE_NUM_MASTER_SLOTS   = count_master_slots();
localparam NODE_MASTER_ID_WIDTH   = (NODE_NUM_MASTER_SLOTS == 1) ? 1 : $clog2(NODE_NUM_MASTER_SLOTS);

//-------------------------------------------------------------------------------
// signals & interfaces
//-------------------------------------------------------------------------------

// Handling node's connectivity to interconnect crossbar matrix
logic [ IC_ARADDR_WIDTH-1       : 0  ] node_araddr_w [ NODE_NUM_MASTER_SLOTS ];
logic [ NODE_NUM_MASTER_SLOTS-1 : 0  ] node_arvalid_w;
logic [ NODE_NUM_MASTER_SLOTS-1 : 0  ] node_arready_w;
logic [ NODE_NUM_MASTER_SLOTS-1 : 0  ] node_rready_w;
logic [ NODE_NUM_MASTER_SLOTS-1 : 0  ] node_rvalid_w;
logic [ IC_RDATA_WIDTH-1        : 0  ] node_rdata_w;
// IDs of masters, which sent requests
logic [ NODE_NUM_MASTER_SLOTS-1 : 0 ] mst_id_reqst_onehot;
logic [ NODE_NUM_MASTER_SLOTS-1 : 0 ] mst_id_reqst_prior_onehot;
logic [ NODE_NUM_MASTER_SLOTS-1 : 0 ] mst_id_reqst_prior_onehot_r;

logic [ NODE_MASTER_ID_WIDTH-1 : 0 ] mst_id_reqst;
logic [ NODE_MASTER_ID_WIDTH-1 : 0 ] mst_id_reqst_prior;
logic [ NODE_MASTER_ID_WIDTH-1 : 0 ] mst_id_reqst_prior_r;


// Signals from/to AXI master
logic [ IC_ARADDR_WIDTH-1      : 0 ] slv_araddr_wo;
logic                                slv_arvalid_wo;
logic                                slv_arready_wi;

logic [3:0]                          node_arqos_w [  NODE_NUM_MASTER_SLOTS ];

logic [ IC_RDATA_WIDTH-1       : 0 ] slv_rdata_wi;
logic                                slv_rvalid_wi;
logic                                slv_rready_wo;

// Flags
logic node_busy;
logic ar_success;
logic ar_success_r;

//-------------------------------------------------------------------------------
// = Reconnect interfaces and combine from interfaces
//-------------------------------------------------------------------------------
assign slv_axil.ar_valid = slv_arvalid_wo; 
assign slv_axil.ar_addr  = slv_araddr_wo;
assign slv_arready_wi    = slv_axil.ar_ready;

assign slv_axil.r_ready = slv_rready_wo;
assign slv_rdata_wi     = {slv_axil.r_data, slv_axil.r_resp};
assign slv_rvalid_wi    = slv_axil.r_valid; 

//-------------------------------------------------------------------------------
// Reconnect crossbar, if nodes has no connection
//-------------------------------------------------------------------------------

assign cbar_resp_data_o = node_rdata_w;
generate 
    for (genvar node_mst_slot_idx = 0; node_mst_slot_idx < NODE_NUM_MASTER_SLOTS; node_mst_slot_idx++) begin
        localparam ic_mst_slot_idx = get_connectivity_idx(node_mst_slot_idx);

        assign node_araddr_w[node_mst_slot_idx] = cbar_reqst_data_i[ic_mst_slot_idx];
        assign node_arqos_w[node_mst_slot_idx] = cbar_reqst_arqos_i[ic_mst_slot_idx];
        assign node_arvalid_w [node_mst_slot_idx] = cbar_reqst_val_i [ic_mst_slot_idx];
        assign node_rready_w  [node_mst_slot_idx] = cbar_resp_rdy_i  [ic_mst_slot_idx];

        assign cbar_reqst_rdy_o[ic_mst_slot_idx]  = node_arready_w[node_mst_slot_idx];
        assign cbar_resp_val_o [ic_mst_slot_idx]  = node_rvalid_w [node_mst_slot_idx];
    end
endgenerate


//-------------------------------------------------------------------------------
// AXI signal management
//-------------------------------------------------------------------------------

// Define mst id, from which the request came
assign mst_id_reqst        = (!node_busy) ? mst_id_reqst_prior : mst_id_reqst_prior_r;
// The same, but onehot
assign mst_id_reqst_onehot = (!node_busy) ? mst_id_reqst_prior_onehot : mst_id_reqst_prior_onehot_r;


// = AR channel = //

assign  slv_arvalid_wo = ((|(node_arvalid_w)) && (!ar_success_r));
assign node_arready_w  = (slv_arready_wi && (!ar_success_r)) ? mst_id_reqst_onehot : '0;
assign  slv_araddr_wo  = node_araddr_w[mst_id_reqst]; 

// = R channel = //

assign node_rvalid_w = (slv_rvalid_wi) ? mst_id_reqst_prior_onehot_r : '0; 
assign slv_rready_wo = |(mst_id_reqst_prior_onehot_r & node_rready_w);
assign node_rdata_w  = slv_rdata_wi;

//-------------------------------------------------------------------------------
// Save id of master, which sent the reqst
//-------------------------------------------------------------------------------


///



///

always_ff @(posedge clk_i or negedge rstn_i)
if      (!rstn_i)                      mst_id_reqst_prior_onehot_r <= '0;
else if (slv_arvalid_wo && !node_busy) mst_id_reqst_prior_onehot_r <= mst_id_reqst_prior_onehot;
else                                   mst_id_reqst_prior_onehot_r <= mst_id_reqst_prior_onehot_r;



always_ff @(posedge clk_i or negedge rstn_i)
if      (!rstn_i)                      mst_id_reqst_prior_r <= '0;
else if (slv_arvalid_wo && !node_busy) mst_id_reqst_prior_r <= mst_id_reqst_prior;
else                                   mst_id_reqst_prior_r <= mst_id_reqst_prior_r;

//-------------------------------------------------------------------------------
// Flags of busy node
//-------------------------------------------------------------------------------

always_ff @(posedge clk_i or negedge rstn_i)
if      (!rstn_i)                       node_busy <= 'b0;
else if (slv_rvalid_wi & slv_rready_wo) node_busy <= 'b0;
else if (|node_arvalid_w              ) node_busy <= 'b1;
else                                    node_busy <= node_busy;

//-------------------------------------------------------------------------------
// Flags of success transactions
//-------------------------------------------------------------------------------

assign ar_success   = slv_arvalid_wo & slv_arready_wi;
always_ff @(posedge clk_i or negedge rstn_i)
if      (!rstn_i)                       ar_success_r <= 'b0;
else if (slv_rvalid_wi & slv_rready_wo) ar_success_r <= 'b0;
else                                    ar_success_r <= ar_success_r | ar_success;



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

logic [31:0]                          node_arqos_shift [  NODE_NUM_MASTER_SLOTS ];

for (genvar j = 0; j < NODE_NUM_MASTER_SLOTS; j = j + 1) begin 
    assign node_arqos_shift[j] = 1 << cbar_reqst_arqos_i[j];
end

logic [32-1:0] mask [5-1:0];
logic [31 : 0] qos_sum;
logic [31 : 0] tmp_or;
logic tmp_or_bit;

always_comb begin
    for (int i = 0; i < 32; i++) begin
        tmp_or_bit = 1'b0;
        for (int j = 0; j < NODE_NUM_MASTER_SLOTS; j++) begin
            tmp_or_bit = tmp_or_bit | node_arqos_shift[j][i];
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
    for(genvar j = 0; j < 32; j = j + 1) begin
        assign mask[i][j] = (j >> i) & 1;
    end
end

assign qos_sum =  tmp_or;

logic [31:0] one_shot_catch;

always_comb begin
    for(int i = 0; i < NODE_NUM_MASTER_SLOTS; i = i + 1) begin    
        mst_id_reqst_prior_onehot = (node_arqos_shift[i] == one_shot_catch)? (1 << i) : 0;
    end
end
for(genvar i = 0; i < NODE_NUM_MASTER_SLOTS; i = i + 1) begin    
    assign mst_id_reqst_prior[i] = |(one_shot_catch & mask[i]);
end

//-------------------------------------------------------------------------------
// initializations units
//-------------------------------------------------------------------------------

liteic_priority_cd #(.IN_WIDTH(NODE_NUM_MASTER_SLOTS), .OUT_WIDTH(NODE_MASTER_ID_WIDTH), .IC_NUM_MASTER_SLOTS(IC_NUM_MASTER_SLOTS)) 
master_reqst_priority_cd (
    //.in     (node_arvalid_w           ),
    .in     (qos_sum                  ),
    .onehot (one_shot_catch)//,
//    .out    (       )
); 
endmodule