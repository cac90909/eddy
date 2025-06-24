from dataclasses import dataclass
from typing import Dict, Tuple, Callable, Any, Type

from core.operation.enums import (
    OperationName as OpName,
    OperationType as OpType,
    OperationArgumentName as OpArgName,
    TraversalDirection
)
from core.operation.domain import ArgumentSpec as ArgSpec
from core.operation.domain import OperationSpec as OpSpec
from core.operation.service import OperationService as OpSvc

# import your DRF serializers
from core.operation.serializers import (
    RawOperationResponseSerializer,
    ListOperationResponseSerializer,
    EnrichedOperationResponseSerializer,
    MetricOperationResponseSerializer,
)

import core.operation.arguments.choices as ChoiceUtil



# Central registry of all operations, now including response serializers and descriptions
OPERATION_SPECS: Dict[OpName, OpSpec] = {
    # ----- Raw Universal -----
    OpName.FULL_DATA: OpSpec(
        name=OpName.FULL_DATA,
        result_type=OpType.RAW,
        args=(),
        service_method=OpSvc.full_data,
        response_serializer=RawOperationResponseSerializer,
        description="Load the full dataset",
    ),
    OpName.FILTER: OpSpec(
        name=OpName.FILTER,
        result_type=OpType.RAW,
        args=(
            ArgSpec(OpArgName.COLUMN, required=True, choices_fn=ChoiceUtil.get_column_name_options),
            ArgSpec(OpArgName.VALUE, required=True, choices_fn=ChoiceUtil.get_column_value_options),
            ArgSpec(OpArgName.OPERATOR, required=True, choices_fn=ChoiceUtil.get_filter_type_options),
        ),
        service_method=OpSvc.filter,
        response_serializer=RawOperationResponseSerializer,
        description="Filter rows by column and value",
    ),
    OpName.TRAVERSE: OpSpec(
        name=OpName.TRAVERSE,
        result_type=OpType.RAW,
        args=(
            ArgSpec(OpArgName.START_ID, required=True, choices_fn=ChoiceUtil.get_start_id_options),
            ArgSpec(OpArgName.DIRECTIONS, required=True, multiple=True, choices={dir for dir in TraversalDirection}),
        ),
        service_method=OpSvc.traverse,
        response_serializer=RawOperationResponseSerializer,
        description="Traverse graph relationships",
    ),

    # ----- Metric (Simple Aggregations) -----
    OpName.SIMPLE_COUNT: OpSpec(
        name=OpName.SIMPLE_COUNT,
        result_type=OpType.METRIC,
        args=(ArgSpec(OpArgName.COLUMN, required=True, choices_fn=ChoiceUtil.get_column_name_options),),
        service_method=OpSvc.simple_count,
        response_serializer=MetricOperationResponseSerializer,
        description="Count rows in a column",
    ),
    OpName.SIMPLE_SUM: OpSpec(
        name=OpName.SIMPLE_SUM,
        result_type=OpType.METRIC,
        args=(ArgSpec(OpArgName.COLUMN, required=True, choices_fn=ChoiceUtil.get_column_name_options),),
        service_method=OpSvc.simple_min,
        response_serializer=MetricOperationResponseSerializer,
        description="Sum values in a column",
    ),
    OpName.SIMPLE_MIN: OpSpec(
        name=OpName.SIMPLE_MIN,
        result_type=OpType.METRIC,
        args=(ArgSpec(OpArgName.COLUMN, required=True, choices_fn=ChoiceUtil.get_column_name_options),),
        service_method=OpSvc.simple_min,
        response_serializer=MetricOperationResponseSerializer,
        description="Minimum value in a column",
    ),
    OpName.SIMPLE_MAX: OpSpec(
        name=OpName.SIMPLE_MAX,
        result_type=OpType.METRIC,
        args=(ArgSpec(OpArgName.COLUMN, required=True, choices_fn=ChoiceUtil.get_column_name_options),),
        service_method=OpSvc.simple_max,
        response_serializer=MetricOperationResponseSerializer,
        description="Maximum value in a column",
    ),
    OpName.SIMPLE_AVERAGE: OpSpec(
        name=OpName.SIMPLE_AVERAGE,
        result_type=OpType.METRIC,
        args=(ArgSpec(OpArgName.COLUMN, required=True, choices_fn=ChoiceUtil.get_column_name_options),),
        service_method=OpSvc.simple_average,
        response_serializer=MetricOperationResponseSerializer,
        description="Average of values in a column",
    ),

    # ----- List Unique Values -----
    OpName.UNIQUE_COLUMN_VALUES: OpSpec(
        name=OpName.UNIQUE_COLUMN_VALUES,
        result_type=OpType.LIST,
        args=(ArgSpec(OpArgName.COLUMN, required=True, choices_fn=ChoiceUtil.get_column_name_options),),
        service_method=OpSvc.unique_column_values,
        response_serializer=ListOperationResponseSerializer,
        description="List distinct values of a column",
    ),
    OpName.UNIQUE_JSON_KEYS: OpSpec(
        name=OpName.UNIQUE_JSON_KEYS,
        result_type=OpType.LIST,
        args=(),
        service_method=OpSvc.unique_json_keys,
        response_serializer=ListOperationResponseSerializer,
        description="List distinct JSON keys",
    ),
    OpName.UNIQUE_JSON_KEY_VALUES: OpSpec(
        name=OpName.UNIQUE_JSON_KEY_VALUES,
        result_type=OpType.LIST,
        args=(ArgSpec(OpArgName.KEY, required=True, choices_fn=ChoiceUtil.get_unique_json_keys),),
        service_method=OpSvc.unique_json_key_values,
        response_serializer=ListOperationResponseSerializer,
        description="List distinct values for a JSON key",
    ),
    OpName.UNIQUE_JSON_VALUES: OpSpec(
        name=OpName.UNIQUE_JSON_VALUES,
        result_type=OpType.LIST,
        args=(),
        service_method=OpSvc.unique_json_values,
        response_serializer=ListOperationResponseSerializer,
        description="List all values in JSON fields",
    ),

    # ----- Group Aggregations (Enriched) -----
    OpName.GROUP_COUNT: OpSpec(
        name=OpName.GROUP_COUNT,
        result_type=OpType.ENRICHED,
        args=(
            ArgSpec(OpArgName.GROUP_COLUMNS, required=True, multiple=True, choices_fn=ChoiceUtil.get_column_name_options),
            ArgSpec(OpArgName.TARGET_COLUMN,  required=True, choices_fn=ChoiceUtil.get_target_column_options),
            ArgSpec(OpArgName.FREQUENCY,      required=False, choices_fn=ChoiceUtil.get_group_aggregate_frequency_options),
        ),
        service_method=OpSvc.group_count,
        response_serializer=EnrichedOperationResponseSerializer,
        description="Count by group over time",
    ),
    OpName.GROUP_SUM: OpSpec(
        name=OpName.GROUP_SUM,
        result_type=OpType.ENRICHED,
        args=(
            ArgSpec(OpArgName.GROUP_COLUMNS, required=True, multiple=True, choices_fn=ChoiceUtil.get_column_name_options),
            ArgSpec(OpArgName.TARGET_COLUMN,  required=True, choices_fn=ChoiceUtil.get_target_column_options),
            ArgSpec(OpArgName.FREQUENCY,      required=False, choices_fn=ChoiceUtil.get_group_aggregate_frequency_options),
        ),
        service_method=OpSvc.group_sum,
        response_serializer=EnrichedOperationResponseSerializer,
        description="Sum by group over time",
    ),
    OpName.GROUP_MIN: OpSpec(
        name=OpName.GROUP_MIN,
        result_type=OpType.ENRICHED,
        args=(
            ArgSpec(OpArgName.GROUP_COLUMNS, required=True, multiple=True, choices_fn=ChoiceUtil.get_column_name_options),
            ArgSpec(OpArgName.TARGET_COLUMN,  required=True, choices_fn=ChoiceUtil.get_target_column_options),
            ArgSpec(OpArgName.FREQUENCY,      required=False, choices_fn=ChoiceUtil.get_group_aggregate_frequency_options),
        ),
        service_method=OpSvc.group_min,
        response_serializer=EnrichedOperationResponseSerializer,
        description="Minimum by group over time",
    ),
    OpName.GROUP_MAX: OpSpec(
        name=OpName.GROUP_MAX,
        result_type=OpType.ENRICHED,
        args=(
            ArgSpec(OpArgName.GROUP_COLUMNS, required=True, multiple=True, choices_fn=ChoiceUtil.get_column_name_options),
            ArgSpec(OpArgName.TARGET_COLUMN,  required=True, choices_fn=ChoiceUtil.get_target_column_options),
            ArgSpec(OpArgName.FREQUENCY,      required=False, choices_fn=ChoiceUtil.get_group_aggregate_frequency_options),
        ),
        service_method=OpSvc.group_max,
        response_serializer=EnrichedOperationResponseSerializer,
        description="Maximum by group over time",
    ),
    OpName.GROUP_AVERAGE: OpSpec(
        name=OpName.GROUP_AVERAGE,
        result_type=OpType.ENRICHED,
        args=(
            ArgSpec(OpArgName.GROUP_COLUMNS, required=True, multiple=True, choices_fn=ChoiceUtil.get_column_name_options),
            ArgSpec(OpArgName.TARGET_COLUMN,  required=True, choices_fn=ChoiceUtil.get_target_column_options),
            ArgSpec(OpArgName.FREQUENCY,      required=False, choices_fn=ChoiceUtil.get_group_aggregate_frequency_options),
        ),
        service_method=OpSvc.group_average,
        response_serializer=EnrichedOperationResponseSerializer,
        description="Average by group over time",
    ),
}