import inspect
from typing import Any, Set, List


from core.domain.common.enums.data_types import DataType
from core.domain.universal.enums.univ_columns import UniversalColumn
from core.domain.universal.maps.univ_col_to_datatype import UNIVERSAL_COLUMN_TO_DATATYPE
from core.domain.universal.maps.dtype_to_op_options import DATATYPE_TO_OPERATOR_OPTIONS 
from core.domain.universal.enums.non_filter_cols import NON_FILTERABLE_COLUMNS
from core.domain.operation.enums.op_name import OperationName
from core.domain.operation.enums.traversal_directions import TraversalDirection
from core.domain.universal.enums.frequency_type import FrequencyType
from core.services.operation import OperationService
import backend.apps.core.infrastructure.repositories.universal_util as UniversalUtil
from core.infrastructure.repositories.universal import UniversalRepository
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
    combined = [*filterable_keys, *filterable_cols]
    return sorted(combined)

def get_column_value_options(user_id, data_src, col_name):
    return UniversalRepository().get_unique_values(data_src, col_name)

def get_filter_type_options(user_id, data_src, col_name):
    col_data_type = UniversalUtil.get_column_primitive_type(data_src, col_name)
    return DATATYPE_TO_OPERATOR_OPTIONS[col_data_type]


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

import inspect
from typing import Any, Callable, Dict

def invoke_choices_fn(
    fn: Callable[..., Any],
    *,
    user_id: int,
    data_src: Any,
    args: Dict[str, Any],
) -> Any:
    """
    Call fn by matching its parameters to (user_id, data_src, args[...]).
    Raises if fn asks for something we didn’t supply.
    """
    sig   = inspect.signature(fn)
    bound = {}
    for name, param in sig.parameters.items():
        if name == "user_id":
            bound[name] = user_id
        elif name == "data_src":
            bound[name] = data_src
        elif name in args:
            bound[name] = args[name]
        else:
            raise ValueError(f"Missing argument {name!r} for {fn.__name__}")
    return fn(**bound)