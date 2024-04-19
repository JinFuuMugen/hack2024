# AXI4-Lite Interconnect

AXI4-Lite Interconnect — это интерконнект, основанный на стандарте AXI4-Lite.
Проект написан для хакатона SoC Design Challenge 2024 года трека RTL и RTL PRO.
Референсный дизайн интерконнекта спроектирован в упрощенном варианте с добавленным приоритетом транзакций.

![readme_interconnect_schem](/doc/img/readme_interconnect_schem.drawio.svg)

## Основные характеристики
- Основан на стандарте AXI4-Lite. Подробнее о семействае стандартов в [этом документе](./doc/AXI.md).
- Тестовая конфигурация 20 master и 12 slave.
- Добавлены сигналы ar_qos и aw_qos для указания приоритета транзакций.
- Настроенное окружение под верификацию.
- Написан на SystemVerilog с использованием интерфейсов. Об интерфейсах можно [прочитать тут](./doc/interfaces.md#interface).

## Содержимое репозитория
|Директория             |Описание                                                             |
|-----------------------|---------------------------------------------------------------------|
|doc                    |Документация на проект                                               |
|rtl                    |Исходные коды ядра интерконнекта                                     |
|dv                     |Верификационное окружение                                            |
|scripts                |Скрипты для создания проекта в Vivado                                |

## Задание

В задании вам необходимо оптимизировать микроархитектуру предложенного интерконнекта по критериям максимальной тактовой частоты, площади, средней задержки и пропускной способности. Формулы расчета и учета результатов участников описаны в [правилах](./doc/RTL_rules.md).

Предлагаемый интерконнект основан на стандарте AXI4-Lite, но имеет существенное отличие: для каждой транзакции, независимо от того, с какого master она сформирована, выставляется приоритет с точки зрения арбитража. За передачу приоритетов отвечает сигнал `ar_qos` для чтения и сигнал `aw_qos` записи. Чем больше значение в этом поле, тем более приоритетна транзакция при арбитраже для доступа к slave.

Учет приоритетов транзакций заложен в критерии оценивания и отражает уникальность задания, которое предстоит решить участникам.

В реализуемой конфигурации интерконнект использует 20 портов для master и 12 портов для slave. Референсная реализация, будучи функционально рабочей, не обеспечивает оптимальную реализацию даже без учета приоритета транзакций. Не будет ошибкой сказать, что это очень плохая реализация, которую необходимо улучшать и дорабатывать. Если совсем непонятно, о чем идет речь, то настоятельно рекомендуется ознакомиться с [документом, поясняющим базовое устройство, назначение и принцип работы интерконнекта](./doc/Interconnect.md).

Обратите внимание на весовые коэффициенты при расчете итогового балла в правилах соревнования. Они отражают важность критериев с точки зрения итоговой оценки и являются субъективным отражением видения организаторов на то, каким должен быть финальный результат.

На рисунке ниже показан **пример** транзакций **чтения**, когда сразу **три ведущих устройства** одновременно обращаются **к одному и тому же ведомому устройству**. `Slave` обрабатывает приходящие данные и выставляет считанные данные согласно заданному приоритету (`ARQOS`). 

![readme_wave](/doc/img/readme_wave.drawio.svg)

## Работа с проектом

Нюансы выполнения описанных ниже пунктов можно узнать в документации, доступной в репозитории.

В данном разделе описаны те возможности и шаги, которые вам надо предпринять, чтобы проверить правильность функционирования вашего решения и оценить полученные характеристики.

Если вы внесли изменение в проект и решили, что они верные, то вам необходимо провести следующие действия:

- Запустите верификацию вашего решения с использованием предоставленного окружения в виртуальной машине и узнайте текущие значения метрик. Вы можете узнать, как это сделать, в [соответствующей документации](./doc/Questa_How_To.md). Проверьте, что проект прошел тест без ошибок. Если нашли ошибки, то вернитесь к исправлению кода. Кроме того, при запуске верификационного окружения можно проверить, как обрабатываются пакеты с различным приоритетом, проанализировав временную диаграмму и логи симуляции.
- Сделайте commit со своим решением в репозиторий и запустите проверку закрытого теста через CI. Подробнее об этом [тут](./doc/CI.md) Получите метрики по задержке и пропускной способности. Для подробной информации по работе с gitlab-ом рекомендуется ознакомиться со [следующим материалом](./doc/git_manual.md). 
- Запустите имплементацию проекта в Vivado через скрипт или GUI. Настоятельно рекомендуется [ознакомиться с материалом по работе со скриптами](./doc/scripts.md) перед попыткой запуска чего-либо. Проверьте, что синтез прошел без ошибок. Получите результаты по частоте и площади. При возникновении вопросов в процессе работы в графической оболочке Vivado, рекомендуется ознакомиться с [соответсвующим материалом](./doc/vivado_manual.md).
- Проверьте, что полученный netlist после синтеза в Vivado проходит тест. Если тест не пройдет после синтеза, значит RTL описание проекта содержит ошибки или синтезируемые конструкции. Изучайте предупреждения в Vivado, чтобы найти проблемы. Важно! Результат решения, не проходящий тест после синтеза, не принимается к оцениванию.

 Отметим, что не обязательно выполнять эти шаги строго последовательно, некоторые из них можно делать параллельно.

Если у вас возникают проблемы с выполнением какого-то из этапов, задайте вопрос организаторам. Вам помогут.

Удачи в решении задачи! У вас все получится!
