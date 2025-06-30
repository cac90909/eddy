from enum import Enum

class OperationName(str, Enum):
    FULL_DATA                = "full_data"
    FILTER                   = "filter"
    TRAVERSE                 = "traverse"
    UNIQUE_COLUMN_VALUES     = "unique_column_values"
    UNIQUE_JSON_KEYS         = "unique_json_keys"
    UNIQUE_JSON_VALUES       = "unique_json_values"
    UNIQUE_JSON_KEY_VALUES   = "unique_json_key_values"

    SIMPLE_COUNT             = "count"
    SIMPLE_AVERAGE           = "average"
    SIMPLE_SUM               = "sum"
    SIMPLE_MIN               = "min"
    SIMPLE_MAX               = "max"

    GROUP_COUNT              = "count_group_aggregate"
    GROUP_MIN                = "min_group_aggregate"
    GROUP_MAX                = "max_group_aggregate"
    GROUP_SUM                = "sum_group_aggregate"
    GROUP_AVERAGE            = "average_group_aggregate"