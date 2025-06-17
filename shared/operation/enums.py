from enum import Enum

class OperationName(str, Enum):
    GET_FULL_DATA               = "get_full_data"
    FILTER                      = "filter"
    TRAVERSE                    = "traverse"
    GET_UNIQUE_COLUMN_VALUES    = "get_unique_column_values"
    GET_UNIQUE_JSON_KEYS        = "get_unique_json_keys"
    GET_UNIQUE_JSON_VALUES      = "get_unique_json_values"
    GET_UNIQUE_JSON_KEY_VALUES  = "get_unique_json_key_values"
    GET_COUNT                   = "get_count"
    GET_AVERAGE                 = "get_average"
    GET_SUM                     = "get_sum"
    GET_MIN                     = "get_min"
    GET_MAX                     = "get_max"
    GET_COUNT_GROUP_AGGREGATE   = "get_count_group_aggregate"
    GET_MIN_GROUP_AGGREGATE     = "get_min_group_aggregate"
    GET_MAX_GROUP_AGGREGATE     = "get_max_group_aggregate"
    GET_SUM_GROUP_AGGREGATE     = "get_sum_group_aggregate"
    GET_AVERAGE_GROUP_AGGREGATE = "get_average_group_aggregate"

class OperationType(str, Enum):
    RAW      = "raw"
    LIST     = "list"
    METRIC   = "metric"
    ENRICHED = "enriched"

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

class TraversalDirection(str, Enum):
    UPWARDS    = "upwards"
    DOWNWARDS  = "downwards"
    HORIZONTAL = "horizontal"
