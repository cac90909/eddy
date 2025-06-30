from dataclasses import dataclass
from typing import Dict, Tuple, Callable, Any, Type
from core.domain.operation.enums.op_name import OperationName as OpName
from core.domain.operation.enums.op_type import OperationType as OpType
from core.domain.operation.enums.op_arg_name import OperationArgumentName as OpArgName
from core.domain.operation.enums.traversal_directions import TraversalDirection
 
from backend.apps.core.domain.operation.structures.operation_spec import (
    ArgumentSpec as ArgSpec,
    OperationSpec as OpSpec
)
from core.domain.operation.maps.arg_name_to_spec import ARGUMENT_SPEC_MAP
from core.services.operation import OperationService as OpSvc

# import your DRF serializers
from core.api.serializers.operation import (
    RawOperationResponseSerializer,
    ListOperationResponseSerializer,
    EnrichedOperationResponseSerializer,
    MetricOperationResponseSerializer,
)

import core.services.operation_arg_choices as ChoiceUtil



# Central registry of all operations, now including response serializers and descriptions
OPERATION_SPECS: Dict[OpName, OpSpec] = {
    # ----- Raw Universal -----
    OpName.FULL_DATA: OpSpec(
        name=OpName.FULL_DATA,
        result_type=OpType.RAW,
        args=(),
        service_method=OpSvc.full_data,
        description="Load the full dataset",
    ),
    OpName.FILTER: OpSpec(
        name=OpName.FILTER,
        result_type=OpType.RAW,
        args=(
            ARGUMENT_SPEC_MAP[OpArgName.COLUMN],
            ARGUMENT_SPEC_MAP[OpArgName.VALUE],
            ARGUMENT_SPEC_MAP[OpArgName.OPERATOR],
        ),
        service_method=OpSvc.filter,
        description="Filter rows by column and value",
    ),
    OpName.TRAVERSE: OpSpec(
        name=OpName.TRAVERSE,
        result_type=OpType.RAW,
        args=(
            ARGUMENT_SPEC_MAP[OpArgName.START_ID],
            ARGUMENT_SPEC_MAP[OpArgName.DIRECTIONS],
        ),
        service_method=OpSvc.traverse,
        description="Traverse graph relationships",
    ),

    # ----- Metric (Simple Aggregations) -----
    OpName.SIMPLE_COUNT: OpSpec(
        name=OpName.SIMPLE_COUNT,
        result_type=OpType.METRIC,
        args=(ARGUMENT_SPEC_MAP[OpArgName.COLUMN],),
        service_method=OpSvc.simple_count,
        description="Count rows in a column",
    ),
    OpName.SIMPLE_SUM: OpSpec(
        name=OpName.SIMPLE_SUM,
        result_type=OpType.METRIC,
        args=(ARGUMENT_SPEC_MAP[OpArgName.COLUMN],),
        service_method=OpSvc.simple_min,
        description="Sum values in a column",
    ),
    OpName.SIMPLE_MIN: OpSpec(
        name=OpName.SIMPLE_MIN,
        result_type=OpType.METRIC,
        args=(ARGUMENT_SPEC_MAP[OpArgName.COLUMN],),
        service_method=OpSvc.simple_min,
        description="Minimum value in a column",
    ),
    OpName.SIMPLE_MAX: OpSpec(
        name=OpName.SIMPLE_MAX,
        result_type=OpType.METRIC,
        args=(ARGUMENT_SPEC_MAP[OpArgName.COLUMN],),
        service_method=OpSvc.simple_max,
        description="Maximum value in a column",
    ),
    OpName.SIMPLE_AVERAGE: OpSpec(
        name=OpName.SIMPLE_AVERAGE,
        result_type=OpType.METRIC,
        args=(ARGUMENT_SPEC_MAP[OpArgName.COLUMN],),
        service_method=OpSvc.simple_average,
        description="Average of values in a column",
    ),

    # ----- List Unique Values -----
    OpName.UNIQUE_COLUMN_VALUES: OpSpec(
        name=OpName.UNIQUE_COLUMN_VALUES,
        result_type=OpType.LIST,
        args=(ARGUMENT_SPEC_MAP[OpArgName.COLUMN],),
        service_method=OpSvc.unique_column_values,
        description="List distinct values of a column",
    ),
    OpName.UNIQUE_JSON_KEYS: OpSpec(
        name=OpName.UNIQUE_JSON_KEYS,
        result_type=OpType.LIST,
        args=(),
        service_method=OpSvc.unique_json_keys,
        description="List distinct JSON keys",
    ),
    OpName.UNIQUE_JSON_KEY_VALUES: OpSpec(
        name=OpName.UNIQUE_JSON_KEY_VALUES,
        result_type=OpType.LIST,
        args=(ARGUMENT_SPEC_MAP[OpArgName.KEY],),
        service_method=OpSvc.unique_json_key_values,
        description="List distinct values for a JSON key",
    ),
    OpName.UNIQUE_JSON_VALUES: OpSpec(
        name=OpName.UNIQUE_JSON_VALUES,
        result_type=OpType.LIST,
        args=(),
        service_method=OpSvc.unique_json_values,
        description="List all values in JSON fields",
    ),

    # ----- Group Aggregations (Enriched) -----
    OpName.GROUP_COUNT: OpSpec(
        name=OpName.GROUP_COUNT,
        result_type=OpType.ENRICHED,
        args=(
            ARGUMENT_SPEC_MAP[OpArgName.GROUP_COLUMNS],
            ARGUMENT_SPEC_MAP[OpArgName.TARGET_COLUMN],
            ARGUMENT_SPEC_MAP[OpArgName.FREQUENCY],
        ),
        service_method=OpSvc.group_count,
        description="Count by group over time",
    ),
    OpName.GROUP_SUM: OpSpec(
        name=OpName.GROUP_SUM,
        result_type=OpType.ENRICHED,
        args=(
            ARGUMENT_SPEC_MAP[OpArgName.GROUP_COLUMNS],
            ARGUMENT_SPEC_MAP[OpArgName.TARGET_COLUMN],
            ARGUMENT_SPEC_MAP[OpArgName.FREQUENCY],
        ),
        service_method=OpSvc.group_sum,
        description="Sum by group over time",
    ),
    OpName.GROUP_MIN: OpSpec(
        name=OpName.GROUP_MIN,
        result_type=OpType.ENRICHED,
        args=(
            ARGUMENT_SPEC_MAP[OpArgName.GROUP_COLUMNS],
            ARGUMENT_SPEC_MAP[OpArgName.TARGET_COLUMN],
            ARGUMENT_SPEC_MAP[OpArgName.FREQUENCY],
        ),
        service_method=OpSvc.group_min,
        description="Minimum by group over time",
    ),
    OpName.GROUP_MAX: OpSpec(
        name=OpName.GROUP_MAX,
        result_type=OpType.ENRICHED,
        args=(
            ARGUMENT_SPEC_MAP[OpArgName.GROUP_COLUMNS],
            ARGUMENT_SPEC_MAP[OpArgName.TARGET_COLUMN],
            ARGUMENT_SPEC_MAP[OpArgName.FREQUENCY],
        ),
        service_method=OpSvc.group_max,
        description="Maximum by group over time",
    ),
    OpName.GROUP_AVERAGE: OpSpec(
        name=OpName.GROUP_AVERAGE,
        result_type=OpType.ENRICHED,
        args=(
            ARGUMENT_SPEC_MAP[OpArgName.GROUP_COLUMNS],
            ARGUMENT_SPEC_MAP[OpArgName.TARGET_COLUMN],
            ARGUMENT_SPEC_MAP[OpArgName.FREQUENCY],
        ),
        service_method=OpSvc.group_average,
        description="Average by group over time",
    ),
}