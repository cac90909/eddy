from typing import Any, Set, List

from core.domain.enums.universal import (
    UniversalColumn,
    DataType,
)
from core.domain.mappings.universal import (
    UNIVERSAL_COLUMN_TO_DATATYPE
)
from core.domain.mappings.operation import (
    DATATYPE_TO_OPERATORS
)
from core.services.operation import OperationService
from core.domain.enums.operation import (
    NON_FILTERABLE_COLUMNS,
    TraversalDirection,
    OperationName,
    FrequencyType
)
import core.util.universal as UniversalUtil
from backend.apps.core.repositories.universal import UniversalRepository
# ----- Raw Universal -----

def get_column_name_options(user_id, data_src) -> List[str]:
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
    filterable_keys = OperationService().unique_json_keys(user_id, data_src)
    combined = filterable_cols | filterable_keys
    return sorted(combined)

def get_column_value_options(user_id, data_src, col_name):
    return UniversalRepository().get_unique_values(data_src, col_name)

def get_filter_type_options(user_id, data_src, col_name):
    col_data_type = UniversalUtil.get_column_primitive_type(data_src, col_name)
    return DATATYPE_TO_OPERATORS[col_data_type]


def get_start_id_options(user_id, data_src):
    """
    Reuses column value options logic but specifically for entry_id.
    """
    return UniversalRepository().get_unique_values(data_src, UniversalColumn.ENTRY_ID)

def get_traversal_direction_options(user_id):
    """
    Always return the fixed set of directions.
    """
    return [d.value for d in TraversalDirection]

def get_unique_json_keys(user_id, data_src):
    return OperationService().unique_json_keys(user_id, data_src)

    
def get_target_column_options(
    user_id: int,
    data_src: Any,
    group_columns: list[str],
    op_name: str
) -> list[str]:
    cols = get_column_name_options(user_id, data_src)
    cols = [c for c in cols if c not in group_columns]
    if op_name != OperationName.GROUP_COUNT:
        valid_types = {DataType.INT, DataType.FLOAT, DataType.DATE}
        return [
          c for c in cols
          if UniversalUtil.get_column_primitive_type(data_src, c) in valid_types
        ]
    return cols

def get_group_aggregate_frequency_options(user_id):
    return [freq.value for freq in FrequencyType]