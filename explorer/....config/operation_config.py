from explorer.cache.service import ExplorerCacheService
from shared.services.universal_raw_service import UniversalRawService
from shared.services.universal_enriched_service import UniversalEnrichedService
from shared.services.universal_metric_service import UniversalMetricService
from shared.services.universal_list_service import UniversalListService
from shared.services.snapshots_service import SnapshotsService
from explorer.util import operation_util

OPERATION_DEFINITIONS = [
    {
        "operation_name": "get_full_data",
        "operation_type": "universal",
        "operation_arguments": [
            {"argument_name": "user_id", "argument_type": "int", "required": True}
        ],
        "operation_result_data_type": "raw",
        "handler": lambda user_id, **kwargs: UniversalRawService().get_full_data(user_id=user_id, **kwargs),
        "cache_policy": lambda op: True,
        "data_source": lambda user_id, op: None,
        "setup": lambda user_id, op: None,
        "display": None,
        "http_method": "GET"
    },
    {
        "operation_name": "filter",
        "operation_type": "universal",
        "operation_arguments": [
            {"argument_name": "user_id", "argument_type": "int", "required": True},
            {
                "argument_name": "column_name",
                "argument_type": "string",
                "required": True,
                "value_options_fetch": lambda user_id, **kwargs: operation_util.get_filterable_column_names(user_id=user_id, **kwargs),
                "value_options_fetch_dependency": None,
            },
            {
                "argument_name": "filter_value",
                "argument_type": "string",
                "required": True,
                "value_options_fetch": lambda user_id, **kwargs: UniversalListService().get_unique_column_values(user_id=user_id, **kwargs),
                "value_options_fetch_dependency": "$column_name",
            },
            {
                "argument_name": "filter_type",
                "argument_type": "string",
                "required": True,
                "value_options_fetch": lambda user_id, **kwargs: operation_util.get_column_operator_options(user_id=user_id, **kwargs),
                "value_options_fetch_dependency": "$column_name",
            }
        ],
        "operation_result_data_type": "raw",
        "handler": lambda user_id, data_source, column_name, filter_value, filter_type, **kwargs: UniversalRawService().filter(
            user_id=user_id, data_source=data_source, column_name=column_name, filter_value=filter_value, filter_type=filter_type, **kwargs
        ),
        "cache_policy": lambda op: True,
        "data_source": lambda user_id, op: ExplorerCacheService().get_most_recent_operation_chain_raw_data_result(user_id=user_id),
        "setup": None,
        "display": "operation_navigation",
        "http_method": "GET"
    },
    {
        "operation_name": "traverse",
        "operation_type": "universal",
        "operation_arguments": [
            {"argument_name": "user_id", "argument_type": "int", "required": True},
            {
                "argument_name": "start_id",
                "argument_type": "int",
                "required": True,
                "value_options_fetch": lambda user_id, **kwargs: UniversalListService().get_unique_column_values(user_id=user_id, **kwargs),
                "value_options_fetch_dependency": "entry_id",
            },
            {
                "argument_name": "traversal_directions",
                "argument_type": "array",
                "required": True,
                "multiple_selection": True,
                "value_options": ["horizontal", "upwards", "downwards"]
            }
        ],
        "operation_result_data_type": "raw",
        "handler": lambda user_id, data_source, start_id, traversal_directions, **kwargs: UniversalRawService().traverse(
            user_id=user_id, data_source=data_source, start_id=start_id, traversal_directions=traversal_directions, **kwargs
        ),
        "cache_policy": lambda op: True,
        "data_source": lambda user_id, op: ExplorerCacheService().get_most_recent_operation_chain_raw_data_result(user_id=user_id),
        "setup": None,
        "display": "operation_navigation",
        "http_method": "GET"
        # Note: For traverse, if you want to use navigation metadata, you might also embed it in the argument objects.
    },
    {
        "operation_name": "group_aggregate",
        "operation_type": "universal",
        "operation_arguments": [
            {"argument_name": "user_id", "argument_type": "int", "required": True},
            {
                "argument_name": "group_columns",
                "argument_type": "string",
                "required": True,
                "multiple_selection": True,
                "value_options_fetch": lambda user_id, **kwargs: operation_util.get_groupable_columns(user_id=user_id, **kwargs),
                "value_options_fetch_dependency": None,
            },
            {
                "argument_name": "aggregate_operation",
                "argument_type": "string",
                "required": True,
                "value_options_fetch": lambda user_id, **kwargs: operation_util.get_aggregate_operation_options(user_id=user_id, **kwargs),
                "value_options_fetch_dependency": "$group_columns",
            },
            {
                "argument_name": "target_column",
                "argument_type": "string",
                "required": True,
                "value_options_fetch": lambda user_id, **kwargs: operation_util.get_aggregate_target_column_options(user_id=user_id, **kwargs),
                "value_options_fetch_dependency": ["$group_columns", "$aggregation_operation"],
            },
            {   "argument_name": "frequency", 
                "argument_type": "string", 
                "required": False,
                "value_options": ["daily", "weekly", "monthly", "yearly"]
            }
        ],
        "operation_result_data_type": "enriched",
        "handler": lambda user_id, **kwargs: UniversalEnrichedService().group_aggregate(user_id=user_id, **kwargs),
        "cache_policy": lambda op: True,
        "data_source": lambda user_id, op: ExplorerCacheService().get_most_recent_operation_chain_raw_data_result(user_id=user_id),
        "setup": None,
        "display": "operation_navigation",
        "http_method": "GET"
    },
  
    {
        "operation_name": "get_unique_column_values",
        "operation_type": "universal",
        "operation_arguments": [
            {"argument_name": "user_id", "argument_type": "int", "required": True},
            {
                "argument_name": "column_name",
                "argument_type": "string",
                "required": True,
                "value_options_fetch": lambda user_id, **kwargs: operation_util.get_filterable_column_names(user_id=user_id, **kwargs),
                "value_options_fetch_dependency": None,
            }
        ],
        "operation_result_data_type": "list",
        "handler": lambda user_id, **kwargs: UniversalListService().get_unique_column_values(user_id=user_id, **kwargs),
        "cache_policy": lambda op: True,
        "data_source": lambda user_id, op: ExplorerCacheService().get_most_recent_operation_chain_raw_data_result(user_id=user_id),
        "setup": None,
        "display": "operation_navigation",
        "http_method": "GET"
    },
    {
        "operation_name": "get_unique_json_keys",
        "operation_type": "universal",
        "operation_arguments": [
            {"argument_name": "user_id", "argument_type": "int", "required": True}
        ],
        "operation_result_data_type": "list",
        "handler": lambda user_id, **kwargs: UniversalListService().get_unique_json_keys(user_id=user_id, **kwargs),
        "cache_policy": lambda op: True,
        "data_source": lambda user_id, op: ExplorerCacheService().get_most_recent_operation_chain_raw_data_result(user_id=user_id),
        "setup": None,
        "display": "operation_navigation",
        "http_method": "GET"
    },
    {
        "operation_name": "get_unique_json_values",
        "operation_type": "universal",
        "operation_arguments": [
            {"argument_name": "user_id", "argument_type": "int", "required": True}
        ],
        "operation_result_data_type": "list",
        "handler": lambda user_id, **kwargs: UniversalListService().get_unique_json_values(user_id=user_id, **kwargs),
        "cache_policy": lambda op: True,
        "data_source": lambda user_id, op: ExplorerCacheService().get_most_recent_operation_chain_raw_data_result(user_id=user_id),
        "setup": None,
        "display": "operation_navigation",
        "http_method": "GET"
    },
    {
        "operation_name": "get_unique_json_key_values",
        "operation_type": "universal",
        "operation_arguments": [
            {"argument_name": "user_id", "argument_type": "int", "required": True},
            {"argument_name": "json_key", "argument_type": "string", "required": True}
        ],
        "operation_result_data_type": "list",
        "handler": lambda user_id, **kwargs: UniversalListService().get_unique_json_key_values(user_id=user_id, **kwargs),
        "cache_policy": lambda op: True,
        "data_source": lambda user_id, op: ExplorerCacheService().get_most_recent_operation_chain_raw_data_result(user_id=user_id),
        "setup": None,
        "display": "operation_navigation",
        "http_method": "GET"
    },
    {
        "operation_name": "get_unique_column_values_filter_options",
        "operation_type": "universal",
        "operation_arguments": [
            {"argument_name": "user_id", "argument_type": "int", "required": True},
            {"argument_name": "column_name", "argument_type": "string", "required": True}
        ],
        "operation_result_data_type": "list",
        "handler": lambda user_id, **kwargs: UniversalListService().get_unique_column_values(user_id=user_id, **kwargs),
        "cache_policy": lambda op: False,
        "data_source": lambda user_id, op: ExplorerCacheService().get_most_recent_operation_chain_raw_data_result(user_id=user_id),
        "setup": None,
        "display": None,
        "http_method": "GET"
    },
    {
        "operation_name": "get_unique_json_keys_filter_options",
        "operation_type": "universal",
        "operation_arguments": [
            {"argument_name": "user_id", "argument_type": "int", "required": True}
        ],
        "operation_result_data_type": "list",
        "handler": lambda user_id, **kwargs: UniversalListService().get_unique_json_keys(user_id=user_id, **kwargs),
        "cache_policy": lambda op: False,
        "data_source": lambda user_id, op: ExplorerCacheService().get_most_recent_operation_chain_raw_data_result(user_id=user_id),
        "setup": None,
        "display": None,
        "http_method": "GET"
    },
    {
        "operation_name": "get_unique_json_values_filter_options",
        "operation_type": "universal",
        "operation_arguments": [
            {"argument_name": "user_id", "argument_type": "int", "required": True}
        ],
        "operation_result_data_type": "list",
        "handler": lambda user_id, **kwargs: UniversalListService().get_unique_json_values(user_id=user_id, **kwargs),
        "cache_policy": lambda op: False,
        "data_source": lambda user_id, op: ExplorerCacheService().get_most_recent_operation_chain_raw_data_result(user_id=user_id),
        "setup": None,
        "display": None,
        "http_method": "GET"
    },
    {
        "operation_name": "get_unique_json_key_values_filter_options",
        "operation_type": "universal",
        "operation_arguments": [
            {"argument_name": "user_id", "argument_type": "int", "required": True},
            {"argument_name": "json_key", "argument_type": "string", "required": True}
        ],
        "operation_result_data_type": "list",
        "handler": lambda user_id, **kwargs: UniversalListService().get_unique_json_key_values(user_id=user_id, **kwargs),
        "cache_policy": lambda op: False,
        "data_source": lambda user_id, op: ExplorerCacheService().get_most_recent_operation_chain_raw_data_result(user_id=user_id),
        "setup": None,
        "display": None,
        "http_method": "GET"
    },
    {
        "operation_name": "get_count",
        "operation_type": "universal",
        "operation_arguments": [
            {"argument_name": "user_id", "argument_type": "int", "required": True},
            {
                "argument_name": "column_name",
                "argument_type": "string",
                "required": True,
                "value_options_fetch": lambda user_id, **kwargs: operation_util.get_filterable_column_names(user_id=user_id, **kwargs),
                "value_options_fetch_dependency": None,
            }
        ],
        "operation_result_data_type": "metric",
        "handler": lambda user_id, **kwargs: UniversalMetricService().get_count(user_id=user_id, **kwargs),
        "cache_policy": lambda op: True,
        "data_source": lambda user_id, op: ExplorerCacheService().get_most_recent_operation_chain_raw_data_result(user_id=user_id),
        "setup": None,
        "display": "operation_navigation",
        "http_method": "GET"
    },
    {
        "operation_name": "get_average",
        "operation_type": "universal",
        "operation_arguments": [
            {"argument_name": "user_id", "argument_type": "int", "required": True},
            {
                "argument_name": "column_name",
                "argument_type": "string",
                "required": True,
                "value_options_fetch": lambda user_id, **kwargs: operation_util.get_filterable_column_names(user_id=user_id, **kwargs),
                "value_options_fetch_dependency": None
            }
        ],
        "operation_result_data_type": "metric",
        "handler": lambda user_id, **kwargs: UniversalMetricService().get_average(user_id=user_id, **kwargs),
        "cache_policy": lambda op: True,
        "data_source": lambda user_id, op: ExplorerCacheService().get_most_recent_operation_chain_raw_data_result(user_id=user_id),
        "setup": None,
        "display": "operation_navigation",
        "http_method": "GET"
    },
    {
        "operation_name": "get_sum",
        "operation_type": "universal",
        "operation_arguments": [
            {"argument_name": "user_id", "argument_type": "int", "required": True},
            {
                "argument_name": "column_name",
                "argument_type": "string",
                "required": True,
                "value_options_fetch": lambda user_id, **kwargs: operation_util.get_filterable_column_names(user_id=user_id, **kwargs),
                "value_options_fetch_dependency": None,
            }
        ],
        "operation_result_data_type": "metric",
        "handler": lambda user_id, **kwargs: UniversalMetricService().get_sum(user_id=user_id, **kwargs),
        "cache_policy": lambda op: True,
        "data_source": lambda user_id, op: ExplorerCacheService().get_most_recent_operation_chain_raw_data_result(user_id=user_id),
        "setup": None,
        "display": "operation_navigation",
        "http_method": "GET"
    },
    {
        "operation_name": "get_min",
        "operation_type": "universal",
        "operation_arguments": [
            {"argument_name": "user_id", "argument_type": "int", "required": True},
            {
                "argument_name": "column_name",
                "argument_type": "string",
                "required": True,
                "value_options_fetch": lambda user_id, **kwargs: operation_util.get_filterable_column_names(user_id=user_id, **kwargs),
                "value_options_fetch_dependency": None,
            }
        ],
        "operation_result_data_type": "metric",
        "handler": lambda user_id, **kwargs: UniversalMetricService().get_min(user_id=user_id, **kwargs),
        "cache_policy": lambda op: True,
        "data_source": lambda user_id, op: ExplorerCacheService().get_most_recent_operation_chain_raw_data_result(user_id=user_id),
        "setup": None,
        "display": "operation_navigation",
        "http_method": "GET"
    },
    {
        "operation_name": "get_max",
        "operation_type": "universal",
        "operation_arguments": [
            {"argument_name": "user_id", "argument_type": "int", "required": True},
            {
                "argument_name": "column_name",
                "argument_type": "string",
                "required": True,
                "value_options_fetch": lambda user_id, **kwargs: operation_util.get_filterable_column_names(user_id=user_id, **kwargs),
                "value_options_fetch_dependency": None,
            }
        ],
        "operation_result_data_type": "metric",
        "handler": lambda user_id, **kwargs: UniversalMetricService().get_max(user_id=user_id, **kwargs),
        "cache_policy": lambda op: True,
        "data_source": lambda user_id, op: ExplorerCacheService().get_most_recent_operation_chain_raw_data_result(user_id=user_id),
        "setup": None,
        "display": "operation_navigation",
        "http_method": "GET"
    },
        {
        "operation_name": "get_operation_chain",
        "operation_type": "universal_util",
        "operation_arguments": [
            {"argument_name": "user_id", "argument_type": "int", "required": True}
        ],
        "operation_result_data_type": "operation_chain",
        "handler": lambda user_id, **kwargs: ExplorerCacheService().get_operation_chain(user_id=user_id, **kwargs),
        "cache_policy": lambda op: False,
        "data_source": lambda user_id, op: None,
        "setup": None,
        "display": None,
        "http_method": "GET"
    },
    {
        "operation_name": "get_operation_chain_operations",
        "operation_type": "universal_util",
        "operation_arguments": [
            {"argument_name": "user_id", "argument_type": "int", "required": True}
        ],
        "operation_result_data_type": "operations_list",
        "handler": lambda user_id, **kwargs: ExplorerCacheService().extract_operation_chain_operations(user_id=user_id, **kwargs),
        "cache_policy": lambda op: False,
        "data_source": lambda user_id, op: None,
        "setup": None,
        "display": None,
        "http_method": "GET"
    },
    {
        "operation_name": "get_operation_chain_results",
        "operation_type": "universal_util",
        "operation_arguments": [
            {"argument_name": "user_id", "argument_type": "int", "required": True}
        ],
        "operation_result_data_type": "results_list",
        "handler": lambda user_id, **kwargs: ExplorerCacheService().extract_operation_chain_result_data(user_id=user_id, **kwargs),
        "cache_policy": lambda op: False,
        "data_source": lambda user_id, op: None,
        "setup": None,
        "display": None,
        "http_method": "GET"
    },
      {
        "operation_name": "get_column_operator_options",
        "operation_type": "universal_util",
        "operation_arguments": [
            {"argument_name": "user_id", "argument_type": "int", "required": True}
        ],
        "operation_result_data_type": "list",
        "handler": lambda user_id, **kwargs: operation_util.get_column_operator_options(user_id=user_id, **kwargs),
        "cache_policy": lambda op: False,
        "data_source": lambda user_id, op: ExplorerCacheService().get_most_recent_operation_chain_raw_data_result(user_id=user_id),
        "setup": None,
        "display": None,
        "http_method": "GET"
    },
    {
        "operation_name": "get_filterable_column_names",
        "operation_type": "universal_util",
        "operation_arguments": [
            {"argument_name": "user_id", "argument_type": "int", "required": True}
        ],
        "operation_result_data_type": "list",
        "handler": lambda user_id, **kwargs: operation_util.get_filterable_column_names(user_id=user_id, **kwargs),
        "cache_policy": lambda op: False,
        "data_source": lambda user_id, op: ExplorerCacheService().get_most_recent_operation_chain_raw_data_result(user_id=user_id),
        "setup": None,
        "display": None,
        "http_method": "GET"
    },
    {
        "operation_name": "get_operation_types",
        "operation_type": "universal_util",
        "operation_arguments": [
            {"argument_name": "user_id", "argument_type": "int", "required": True}
        ],
        "operation_result_data_type": "list",
        "handler": lambda user_id, **kwargs: operation_util.get_operation_result_data_types(user_id=user_id, **kwargs),
        "cache_policy": lambda op: False,
        "data_source": lambda user_id, op: None,
        "setup": None,
        "display": None,
        "http_method": "GET"
    },
        {
        "operation_name": "get_operation_names_for_result_data_type",
        "operation_type": "universal_util",
        "operation_arguments": [
            {"argument_name": "user_id", "argument_type": "int", "required": True},
            {"argument_name": "operation_result_data_type", "argument_type": "string", "required": True}
        ],
        "operation_result_data_type": "list",
        "handler": lambda user_id, **kwargs: operation_util.get_operation_names_for_result_data_type(user_id=user_id, **kwargs),
        "cache_policy": lambda op: False,
        "data_source": lambda user_id, op: None,
        "setup": None,
        "display": None,
        "http_method": "GET"
    },
    {
    "operation_name": "get_operation_argument_names",
    "operation_type": "universal_util",
    "operation_arguments": [
        {"argument_name": "user_id", "argument_type": "int", "required": True},
        {"argument_name": "operation_name", "argument_type": "string", "required": True}
    ],
    "operation_result_data_type": "list",
    "handler": lambda user_id, **kwargs: operation_util.get_operation_argument_names(user_id=user_id, **kwargs),
    "cache_policy": lambda op: False,
    "data_source": lambda user_id, op: None,
    "setup": None,
    "display": None,
    "http_method": "GET"
    },
    {
    "operation_name": "get_operation_argument_options",
    "operation_type": "universal_util",
    "operation_arguments": [
        {"argument_name": "user_id", "argument_type": "int", "required": True},
        {"argument_name": "operation_name", "argument_type": "string", "required": True},
        {"argument_name": "operation_argument_name", "argument_type": "string", "required": True},
        {"argument_name": "prev_argument_values", "argument_type": "string", "required": True}
    ],
    "operation_result_data_type": "list",
    "handler": lambda user_id, **kwargs: operation_util.get_operation_argument_options(user_id=user_id, **kwargs),
    "cache_policy": lambda op: False,
    "data_source": lambda user_id, op: None,
    "setup": None,
    "display": "operation_argument_options",
    "http_method": "GET"
    },
    {
        "operation_name": "start_explorer_session",
        "operation_type": "state",
        "operation_arguments": [
            {"argument_name": "user_id", "argument_type": "int", "required": True}
        ],
        "operation_result_data_type": "config",
        "handler": lambda user_id, **kwargs: operation_util.assemble_operations_config(user_id=user_id, **kwargs),
        "cache_policy": lambda op: False,
        "data_source": lambda user_id, op: None,
        "setup": lambda user_id, **kwargs: ExplorerCacheService().create_empty_operation_chain_cache(user_id=user_id, **kwargs),
        "display": None,
        "http_method": "GET"
    },
    {
        "operation_name": "reset_explorer_session",
        "operation_type": "state",
        "operation_arguments": [
            {"argument_name": "user_id", "argument_type": "int", "required": True}
        ],
        "operation_result_data_type": "raw",
        "handler": lambda user_id, **kwargs: UniversalRawService().get_full_data(user_id=user_id, **kwargs),
        "cache_policy": lambda op: True,
        "data_source": lambda user_id, op: None,
        "setup": lambda user_id, op: ExplorerCacheService().create_empty_operation_chain_cache(user_id=user_id),
        "display": "reset_button",
        "http_method": "GET"
    },
    {
        "operation_name": "undo_operation",
        "operation_type": "state",
        "operation_arguments": [
            {"argument_name": "user_id", "argument_type": "int", "required": True}
        ],
        "operation_result_data_type": "raw",
        "handler": lambda user_id, **kwargs: ExplorerCacheService().get_most_recent_operation_chain_raw_data_result(user_id=user_id, **kwargs),
        "cache_policy": lambda op: False,
        "data_source": lambda user_id, op: None,
        "setup": lambda user_id, op: ExplorerCacheService().delete_most_recent_operation_from_chain(user_id=user_id),
        "precondition": {
            "condition": lambda user_id: len(ExplorerCacheService().get_operation_chain(user_id=user_id)) > 1,
            "error_message": "Cannot undo operation: no operations to undo"
        },
        "display": "undo_button",
        "http_method": "POST"
    },
    {
        "operation_name": "load_snapshot",
        "operation_type": "state",
        "operation_arguments": [
            {"argument_name": "user_id", "argument_type": "int", "required": True},
            {"argument_name": "snapshot_id", "argument_type": "string", "required": True}
        ],
        "operation_result_data_type": "raw",
        "handler": lambda user_id, **kwargs: operation_util.assemble_dataset_list_from_operation_chain(user_id, operation_chain=kwargs.get("data_source")),
        "cache_policy": lambda op: False,
        "data_source": lambda user_id, op: SnapshotsService().get_snapshot_operation_chain(
            user_id=user_id,
            snapshot_id=op.operation_arguments["snapshot_id"]
        ),
        "setup": lambda user_id, op: ExplorerCacheService().empty_operation_chain(user_id=user_id),
        "display": "load_button",
        "http_method": "GET"
    },
    {
        "operation_name": "end_explorer_session",
        "operation_type": "state",
        "operation_arguments": [
            {"argument_name": "user_id", "argument_type": "int", "required": True}
        ],
        "operation_result_data_type": "status",
        "handler": lambda user_id, **kwargs: ExplorerCacheService().delete_operation_chain_cache(user_id=userId, **kwargs),
        "cache_policy": lambda op: False,
        "data_source": lambda user_id, op: None,
        "setup": None,
        "display": None,
        "http_method": "POST"
    },
    {
        "operation_name": "save_snapshot",
        "operation_type": "snapshot",
        "operation_arguments": [
            {"argument_name": "user_id", "argument_type": "int", "required": True},
            {"argument_name": "title", "argument_type": "string", "required": True},
            {"argument_name": "description", "argument_type": "string", "required": False}
        ],
        "operation_result_data_type": "snapshot",
        "handler": lambda user_id, data_source, **kwargs: SnapshotsService().create_snapshot(user_id=user_id, operation_chain=data_source, **kwargs),
        "cache_policy": lambda op: False,
        "data_source": lambda user_id, op: ExplorerCacheService().extract_operation_chain_operations(user_id=user_id),
        "setup": None,
        "display": "save_button",
        "http_method": "POST"
    },
    {
        "operation_name": "delete_snapshot",
        "operation_type": "snapshot",
        "operation_arguments": [
            {"argument_name": "user_id", "argument_type": "int", "required": True},
            {"argument_name": "snapshot_id", "argument_type": "string", "required": True}
        ],
        "operation_result_data_type": "status",
        "handler": lambda user_id, **kwargs: SnapshotsService().delete_snapshot(user_id=user_id, **kwargs),
        "cache_policy": lambda op: False,
        "data_source": lambda user_id, op: None,
        "setup": "delete_button",
        "http_method": "DELETE"
    },
    {
        "operation_name": "update_snapshot",
        "operation_type": "snapshot",
        "operation_arguments": [
            {"argument_name": "user_id", "argument_type": "int", "required": True},
            {"argument_name": "snapshot_id", "argument_type": "string", "required": True},
            {"argument_name": "title", "argument_type": "string", "required": False},
            {"argument_name": "description", "argument_type": "string", "required": False}
        ],
        "operation_result_data_type": "snapshot",
        "handler": lambda user_id, **kwargs: SnapshotsService().update_snapshot(user_id=user_id, **kwargs),
        "cache_policy": lambda op: False,
        "data_source": lambda user_id, op: None,
        "setup": None,
        "display": "update_button",
        "http_method": "PUT"
    },
    {
        "operation_name": "get_snapshot",
        "operation_type": "snapshot",
        "operation_arguments": [
            {"argument_name": "user_id", "argument_type": "int", "required": True},
            {"argument_name": "snapshot_id", "argument_type": "int", "required": True}
        ],
        "operation_result_data_type": "snapshot",
        "handler": lambda user_id, **kwargs: SnapshotsService().get_snapshot(user_id=user_id, **kwargs),
        "cache_policy": lambda op: False,
        "data_source": lambda user_id, op: None,
        "setup": None,
        "display": None,
        "http_method": "GET"
    },
    {
        "operation_name": "get_all_snapshots",
        "operation_type": "snapshot",
        "operation_arguments": [
            {"argument_name": "user_id", "argument_type": "int", "required": True}
        ],
        "operation_result_data_type": "snapshot_list",
        "handler": lambda user_id, **kwargs: SnapshotsService().get_all_snapshots(user_id=user_id, **kwargs),
        "cache_policy": lambda op: False,
        "data_source": lambda user_id, op: None,
        "setup": None,
        "display": "load_options",
        "http_method": "GET"
    }
]
