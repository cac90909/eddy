from enum import Enum

class OperationArgumentName(str, Enum):
    COLUMN                = "column_name"
    VALUE                 = "filter_value"
    OPERATOR              = "operator"
    START_ID              = "start_id"
    DIRECTIONS            = "directions"
    KEY                   = "key"
    GROUP_COLUMNS         = "group_columns"
    AGGREGATE_OPERATION   = "aggregate_operation"
    TARGET_COLUMN         = "target_column"
    FREQUENCY             = "frequency"
    DATA_SOURCE           = "data_source"