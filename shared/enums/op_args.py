# explorer/enums/arg_names.py
from enum import Enum

class OperationArgumentName(str, Enum):
    COLUMN_NAME           = "column_name"
    FILTER_VALUE          = "filter_value"
    FILTER_TYPE           = "filter_type"
    START_ID              = "start_id"
    TRAVERSAL_DIRECTIONS  = "traversal_directions"
    JSON_KEY              = "json_key"
    GROUP_COLUMNS         = "group_columns"
    AGGREGATE_OPERATION   = "aggregate_operation"
    TARGET_COLUMN         = "target_column"
    FREQUENCY             = "frequency"
