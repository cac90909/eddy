# explorer/enums/operations.py
from enum import Enum

class OperationName(str, Enum):
    GET_FULL_DATA              = "get_full_data"
    FILTER                     = "filter"
    TRAVERSE                   = "traverse"
    GET_UNIQUE_COLUMN_VALUES   = "get_unique_column_values"
    GET_UNIQUE_JSON_KEYS       = "get_unique_json_keys"
    GET_UNIQUE_JSON_VALUES     = "get_unique_json_values"
    GET_UNIQUE_JSON_KEY_VALUES = "get_unique_json_key_values"
    GET_COUNT                  = "get_count"
    GET_AVERAGE                = "get_average"
    GET_SUM                    = "get_sum"
    GET_MIN                    = "get_min"
    GET_MAX                    = "get_max"
    GROUP_AGGREGATE            = "group_aggregate"
