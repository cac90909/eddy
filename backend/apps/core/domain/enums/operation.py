from enum import Enum
from backend.apps.core.enums.universal import UniversalColumn

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

class OperationType(str, Enum):
    RAW      = "raw"
    LIST     = "list"
    METRIC   = "metric"
    ENRICHED = "enriched"

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

class TraversalDirection(str, Enum):
    UPWARDS    = "upwards"
    DOWNWARDS  = "downwards"
    HORIZONTAL = "horizontal"

NON_FILTERABLE_COLUMNS = {
    UniversalColumn.USER.value,
    UniversalColumn.CHILDREN_IDS.value,
    UniversalColumn.PARENTS_IDS.value,
    UniversalColumn.SIBLINGS_IDS.value,
    UniversalColumn.FIELDS.value
}

class FrequencyType(str, Enum):
    DAILY   = "daily"
    WEEKLY  = "weekly"
    MONTHLY = "monthly"
    YEARLY  = "yearly"

class AggregationType(str, Enum):
    COUNT = "count"
    AVG   = "avg"
    SUM   = "sum"
    MIN   = "min"
    MAX   = "max"
