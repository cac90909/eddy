import dataclasses
from typing import Dict, Optional, Any

from shared.operation.enums import (
    OperationName as OpName,
    OperationType as OpType,
    OperationArgumentName as OpArgName,
)
from shared.operation.domain import (
    ArgumentSpec as ArgSpec,
    OperationSpec as OpSpec,
)
from shared.operation.service.service import OperationService
import shared.operation.arguments.choices as ChoiceUtil

# Placeholder VALID and options_fn
DEFAULT_VALIDATOR      = VALID = lambda v, ctx: True
DEFAULT_ERROR_MESSAGE  = ERR_MSG = ""
DEFAULT_OPTIONS_FN     = OPTS = None

# Unbound references to OperationService methods
OpSvc = OperationService

# Core specs remain framework-agnostic; serializers are wired in at the app level.
OP_SPECS: Dict[OpName, OpSpec] = {
    # ----- Raw Universal -----
    OpName.GET_FULL_DATA: OpSpec(
        name           = OpName.GET_FULL_DATA,
        result_type    = OpType.RAW,
        args           = {},
        service_method = OpSvc.get_full_data,
    ),

    OpName.FILTER: OpSpec(
        name           = OpName.FILTER,
        result_type    = OpType.RAW,
        args           = {
            OpArgName.COLUMN_NAME:  ArgSpec(OpArgName.COLUMN_NAME, True, choices_fn=ChoiceUtil.get_column_name_options),
            OpArgName.FILTER_VALUE: ArgSpec(OpArgName.FILTER_VALUE, True, choices_fn=ChoiceUtil.get_column_value_options),
            OpArgName.FILTER_TYPE:  ArgSpec(OpArgName.FILTER_TYPE, True, choices_fn=ChoiceUtil.get_filter_type_options),
        },
        service_method = OpSvc.filter,
    ),

    OpName.TRAVERSE: OpSpec(
        name           = OpName.TRAVERSE,
        result_type    = OpType.RAW,
        args           = {
            OpArgName.START_ID:             ArgSpec(OpArgName.START_ID, True, choices_fn=ChoiceUtil.get_start_id_options),
            OpArgName.TRAVERSAL_DIRECTIONS: ArgSpec(OpArgName.TRAVERSAL_DIRECTIONS, True, choices_fn=ChoiceUtil.get_traversal_direction_options, multiple=True),
        },
        service_method = OpSvc.traverse,
    ),

    # ----- Metric (Simple Aggregations) -----
    OpName.GET_COUNT: OpSpec(
        name           = OpName.GET_COUNT,
        result_type    = OpType.METRIC,
        args           = { OpArgName.COLUMN_NAME: ArgSpec(OpArgName.COLUMN_NAME, True, choices_fn=ChoiceUtil.get_column_name_options) },
        service_method = OpSvc.get_count,
    ),
    OpName.GET_SUM: OpSpec(
        name           = OpName.GET_SUM,
        result_type    = OpType.METRIC,
        args           = { OpArgName.COLUMN_NAME: ArgSpec(OpArgName.COLUMN_NAME, True, choices_fn=ChoiceUtil.get_column_name_options) },
        service_method = OpSvc.get_sum,
    ),
    OpName.GET_MIN: OpSpec(
        name           = OpName.GET_MIN,
        result_type    = OpType.METRIC,
        args           = { OpArgName.COLUMN_NAME: ArgSpec(OpArgName.COLUMN_NAME, True, choices_fn=ChoiceUtil.get_column_name_options) },
        service_method = OpSvc.get_min,
    ),
    OpName.GET_MAX: OpSpec(
        name           = OpName.GET_MAX,
        result_type    = OpType.METRIC,
        args           = { OpArgName.COLUMN_NAME: ArgSpec(OpArgName.COLUMN_NAME, True, choices_fn=ChoiceUtil.get_column_name_options) },
        service_method = OpSvc.get_max,
    ),
    OpName.GET_AVERAGE: OpSpec(
        name           = OpName.GET_AVERAGE,
        result_type    = OpType.METRIC,
        args           = { OpArgName.COLUMN_NAME: ArgSpec(OpArgName.COLUMN_NAME, True, choices_fn=ChoiceUtil.get_column_name_options) },
        service_method = OpSvc.get_average,
    ),

    # ----- List Unique Values -----
    OpName.GET_UNIQUE_COLUMN_VALUES: OpSpec(
        name           = OpName.GET_UNIQUE_COLUMN_VALUES,
        result_type    = OpType.LIST,
        args           = { OpArgName.COLUMN_NAME: ArgSpec(OpArgName.COLUMN_NAME, True, choices_fn=ChoiceUtil.get_column_name_options) },
        service_method = OpSvc.get_unique_column_values,
    ),
    OpName.GET_UNIQUE_JSON_KEYS: OpSpec(
        name           = OpName.GET_UNIQUE_JSON_KEYS,
        result_type    = OpType.LIST,
        args           = {},
        service_method = OpSvc.get_unique_json_keys,
    ),
    OpName.GET_UNIQUE_JSON_KEY_VALUES: OpSpec(
        name           = OpName.GET_UNIQUE_JSON_KEY_VALUES,
        result_type    = OpType.LIST,
        args           = { OpArgName.JSON_KEY: ArgSpec(OpArgName.JSON_KEY, True, choices_fn=ChoiceUtil.get_unique_json_keys) },
        service_method = OpSvc.get_unique_json_key_values,
    ),
    OpName.GET_UNIQUE_JSON_VALUES: OpSpec(
        name           = OpName.GET_UNIQUE_JSON_VALUES,
        result_type    = OpType.LIST,
        args           = {},
        service_method = OpSvc.get_unique_json_values,
    ),

    # ----- Group Aggregations (split functions) -----
    OpName.GET_COUNT_GROUP_AGGREGATE: OpSpec(
        name           = OpName.GET_COUNT_GROUP_AGGREGATE,
        result_type    = OpType.ENRICHED,
        args           = {
            OpArgName.GROUP_COLUMNS: ArgSpec(OpArgName.GROUP_COLUMNS, True, multiple=True, choices_fn=ChoiceUtil.get_column_name_options),
            OpArgName.TARGET_COLUMN:  ArgSpec(OpArgName.TARGET_COLUMN, True, choices_fn=ChoiceUtil.get_target_column_options),
            OpArgName.FREQUENCY:      ArgSpec(OpArgName.FREQUENCY, False, choices_fn=ChoiceUtil.get_group_aggregate_frequency_options),
        },
        service_method = OpSvc.get_count_group_aggregate,
    ),
    OpName.GET_MIN_GROUP_AGGREGATE: OpSpec(
        name           = OpName.GET_MIN_GROUP_AGGREGATE,
        result_type    = OpType.ENRICHED,
        args           = {
            OpArgName.GROUP_COLUMNS: ArgSpec(OpArgName.GROUP_COLUMNS, True, multiple=True, choices_fn=ChoiceUtil.get_column_name_options),
            OpArgName.TARGET_COLUMN:  ArgSpec(OpArgName.TARGET_COLUMN, True, choices_fn=ChoiceUtil.get_target_column_options),
            OpArgName.FREQUENCY:      ArgSpec(OpArgName.FREQUENCY, False, choices_fn=ChoiceUtil.get_group_aggregate_frequency_options),
        },
        service_method = OpSvc.get_min_group_aggregate,
    ),
    OpName.GET_MAX_GROUP_AGGREGATE: OpSpec(
        name           = OpName.GET_MAX_GROUP_AGGREGATE,
        result_type    = OpType.ENRICHED,
        args           = {
            OpArgName.GROUP_COLUMNS: ArgSpec(OpArgName.GROUP_COLUMNS, True, multiple=True, choices_fn=ChoiceUtil.get_column_name_options),
            OpArgName.TARGET_COLUMN:  ArgSpec(OpArgName.TARGET_COLUMN, True, choices_fn=ChoiceUtil.get_target_column_options),
            OpArgName.FREQUENCY:      ArgSpec(OpArgName.FREQUENCY, False, choices_fn=ChoiceUtil.get_group_aggregate_frequency_options),
        },
        service_method = OpSvc.get_max_group_aggregate,
    ),
    OpName.GET_SUM_GROUP_AGGREGATE: OpSpec(
        name           = OpName.GET_SUM_GROUP_AGGREGATE,
        result_type    = OpType.ENRICHED,
        args           = {
            OpArgName.GROUP_COLUMNS: ArgSpec(OpArgName.GROUP_COLUMNS, True, multiple=True, choices_fn=ChoiceUtil.get_column_name_options),
            OpArgName.TARGET_COLUMN:  ArgSpec(OpArgName.TARGET_COLUMN, True, choices_fn=ChoiceUtil.get_target_column_options),
            OpArgName.FREQUENCY:      ArgSpec(OpArgName.FREQUENCY, False, choices_fn=ChoiceUtil.get_group_aggregate_frequency_options),
        },
        service_method = OpSvc.get_sum_group_aggregate,
    ),
    OpName.GET_AVERAGE_GROUP_AGGREGATE: OpSpec(
        name           = OpName.GET_AVERAGE_GROUP_AGGREGATE,
        result_type    = OpType.ENRICHED,
        args           = {
            OpArgName.GROUP_COLUMNS: ArgSpec(OpArgName.GROUP_COLUMNS, True, multiple=True, choices_fn=ChoiceUtil.get_column_name_options),
            OpArgName.TARGET_COLUMN:  ArgSpec(OpArgName.TARGET_COLUMN, True, choices_fn=ChoiceUtil.get_target_column_options),
            OpArgName.FREQUENCY:      ArgSpec(OpArgName.FREQUENCY, False, choices_fn=ChoiceUtil.get_group_aggregate_frequency_options),
        },
        service_method = OpSvc.get_average_group_aggregate,
    ),
}
