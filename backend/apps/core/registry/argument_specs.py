from typing import Dict, Iterable, Sequence, Callable, Any, Set, Optional, Union, Any
from datetime import date
from dataclasses import dataclass

from core.domain.enums.operation import (
    OperationArgumentName as OpArgName,
    TraversalDirection,
)
from core.domain.operation_spec import ArgumentSpec
import core.services.operation_arg_choices as ChoiceUtil

ARGUMENT_SPECS: Dict[OpArgName, ArgumentSpec] = {
    # Basic column + filtering args
    OpArgName.COLUMN: ArgumentSpec(
        name=OpArgName.COLUMN,
        dtype=str,
        required=True,
        multiple=False,
        choices_fn=ChoiceUtil.get_column_name_options,
    ),
    OpArgName.VALUE: ArgumentSpec(
        name=OpArgName.VALUE,
        dtype=str,
        required=True,
        multiple=False,
        choices_fn=ChoiceUtil.get_column_value_options,
    ),
    OpArgName.OPERATOR: ArgumentSpec(
        name=OpArgName.OPERATOR,
        dtype=str,
        required=True,
        multiple=False,
        choices_fn=ChoiceUtil.get_filter_type_options,
    ),

    # Graph traversal
    OpArgName.START_ID: ArgumentSpec(
        name=OpArgName.START_ID,
        dtype=str,
        required=True,
        multiple=False,
        choices_fn=ChoiceUtil.get_start_id_options,
    ),
    OpArgName.DIRECTIONS: ArgumentSpec(
        name=OpArgName.DIRECTIONS,
        dtype=str,
        required=True,
        multiple=True,
        choices=set(TraversalDirection),
    ),

    # JSON/key lookups
    OpArgName.KEY: ArgumentSpec(
        name=OpArgName.KEY,
        dtype=str,
        required=True,
        multiple=False,
        choices_fn=ChoiceUtil.get_unique_json_keys,
    ),

    # Group & target columns
    OpArgName.GROUP_COLUMNS: ArgumentSpec(
        name=OpArgName.GROUP_COLUMNS,
        dtype=str,
        required=True,
        multiple=True,
        choices_fn=ChoiceUtil.get_column_name_options,
    ),
    OpArgName.TARGET_COLUMN: ArgumentSpec(
        name=OpArgName.TARGET_COLUMN,
        dtype=str,
        required=True,
        multiple=False,
        choices_fn=ChoiceUtil.get_target_column_options,
    ),

    # Time‐based aggregations
    OpArgName.FREQUENCY: ArgumentSpec(
        name=OpArgName.FREQUENCY,
        dtype=str,
        required=False,
        multiple=False,
        choices_fn=ChoiceUtil.get_group_aggregate_frequency_options,
    ),

    # Data source selector (if you need it)
    OpArgName.DATA_SOURCE: ArgumentSpec(
        name=OpArgName.DATA_SOURCE,
        dtype=Any,
        required=False,
        multiple=False,
        choices_fn=None,
    ),
}