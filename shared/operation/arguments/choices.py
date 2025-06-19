from typing import set, Any

from shared.universal.enums import (
    UniversalColumn,
    DataType,
)
from shared.universal.mappings import (
    UNIVERSAL_COLUMN_TO_DATATYPE,
    DATATYPE_TO_VALUE_PROVIDER
)
from shared.operation.mappings import (
    DATATYPE_TO_OPERATORS
)
from shared.operation.service import OperationService
from shared.operation.enums import (
    NON_FILTERABLE_COLUMNS,
    TraversalDirection,
    OperationName,
    FrequencyType
)
import shared.universal.util as UniversalUtil
# ----- Raw Universal -----

def get_column_name_options(user_id, data_src) -> set:
    """
    Retrieves list of valid column names that can have a filter operation applied:
    combining a list of Universal Columns + Universal Fields Column Keys 
    (which are drawn from the passed current data source)
    """
    #NOTE: expand scope to just column names instead of filter use case
    #      just naming and invoking all cols instead of non filt cols
    #      esp since non filt cols isnt a thing i think 
    #      (all ops dont use user maybe fields, thats it)
    all_cols = {col.value for col in UniversalColumn}
    filterable_cols = all_cols - NON_FILTERABLE_COLUMNS 
    filterable_keys = OperationService.get_unique_json_keys(user_id, data_src)
    return sorted(filterable_cols + filterable_keys)

def get_column_value_options(user_id, data_src, col_name):
    col_data_type = UniversalUtil.get_column_data_type(data_src, col_name)
    options_provider = DATATYPE_TO_VALUE_PROVIDER[col_data_type]
    return list(options_provider(user_id, data_src, col_name))

def get_filter_type_options(user_id, data_src, col_name):
    col_data_type = UniversalUtil.get_column_data_type(data_src, col_name)
    return DATATYPE_TO_OPERATORS[col_data_type]

def get_start_id_options(user_id, data_src):
    """
    Reuses column value options logic but specifically for entry_id.
    """
    entry_id_dtype = UNIVERSAL_COLUMN_TO_DATATYPE[UniversalColumn.ENTRY_ID]
    options_provider = DATATYPE_TO_VALUE_PROVIDER[entry_id_dtype]
    return list(options_provider(user_id, data_src, UniversalColumn.ENTRY_ID))

def get_traversal_direction_options(user_id):
    """
    Always return the fixed set of directions.
    """
    return [d.value for d in TraversalDirection]

def get_unique_json_keys(user_id, data_src):
    return OperationService.get_unique_json_keys(user_id, data_src)

    
def get_target_column_options(
    user_id: int,
    data_src: Any,
    group_columns: list[str],
    op_name: str
) -> list[str]:
    # base list of all real columns + JSON keys:
    cols = get_column_name_options(user_id, data_src)
    # remove the grouping columns
    cols = [c for c in cols if c not in group_columns]

    # only for non-count ops, further restrict to numeric/date
    if op_name != OperationName.GET_COUNT_GROUP_AGGREGATE.value:
        valid_types = {DataType.INT, DataType.FLOAT, DataType.DATE}
        return [
          c for c in cols
          if UniversalUtil.get_column_data_type(data_src, c) in valid_types
        ]

    # for count, return all
    return cols

def get_group_aggregate_frequency_options(user_id):
    return [freq.value for freq in FrequencyType]