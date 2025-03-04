from explorer.services.explorer_cache_service import ExplorerCacheService
from shared.services.universal_raw_service import UniversalRawService
from shared.services.universal_enriched_service import UniversalEnrichedService
from shared.services.universal_metric_service import UniversalMetricService
from shared.services.universal_list_service import UniversalListService
from shared.services.snapshots_service import SnapshotsService
from explorer.util import operation_util

OPERATION_DEFINITIONS = {
    # Universal Operations
    "init_user": {
        "operation_type": "universal",
        "operation_expected_arguments": ["user_id"],
        "operation_expected_result_data_type": "raw",
        "handler": lambda user_id, **kwargs: UniversalRawService().get_user_universal(user_id=user_id, **kwargs),
        "cache_policy": lambda op: True,
        "data_source": lambda user_id, op: None,
        "setup": lambda user_id, op: ExplorerCacheService().create_empty_operation_chain_cache(user_id=user_id)
    },
    "reset": {
        "operation_type": "universal",
        "operation_expected_arguments": ["user_id"],
        "operation_expected_result_data_type": "raw",
        "handler": lambda user_id, **kwargs: UniversalRawService().get_user_universal(user_id=user_id, **kwargs),
        "cache_policy": lambda op: True,
        "data_source": lambda user_id, op: None,
        "setup": lambda user_id, op: ExplorerCacheService().create_empty_operation_chain_cache(user_id=user_id)
    },
    "filter": {
        "operation_type": "universal",
        "operation_expected_arguments": ["user_id", "column_name", "filter_value", "filter_type"],
        "operation_expected_result_data_type": "raw",
        "handler": lambda user_id, data_source, column_name, filter_value, filter_type, **kwargs: UniversalRawService().filter(
            user_id=user_id, data_source=data_source, column_name=column_name, filter_value=filter_value, filter_type=filter_type, **kwargs
        ),
        "cache_policy": lambda op: True,
        "data_source": lambda user_id, op: ExplorerCacheService().get_most_recent_operation_chain_raw_data_result(user_id=user_id),
        "setup": None
    },
    "traverse": {
        "operation_type": "universal",
        "operation_expected_arguments": ["user_id", "start_id", "traversal_directions"],
        "operation_expected_result_data_type": "raw",
        "handler": lambda user_id, data_source, start_id, traversal_directions, **kwargs: UniversalRawService().traverse(
            user_id=user_id, data_source=data_source, start_id=start_id, traversal_directions=traversal_directions, **kwargs
        ),
        "cache_policy": lambda op: True,
        "data_source": lambda user_id, op: ExplorerCacheService().get_most_recent_operation_chain_raw_data_result(user_id=user_id),
        "setup": None
    },
    "undo": {
        "operation_type": "universal",
        "operation_expected_arguments": ["user_id"],
        "operation_expected_result_data_type": "raw",
        "handler": lambda user_id, **kwargs: ExplorerCacheService().get_most_recent_operation_chain_raw_data_result(user_id=user_id, **kwargs),
        "cache_policy": lambda op: False,
        "data_source": lambda user_id, op: None,
        "setup": lambda user_id, op: ExplorerCacheService().delete_most_recent_operation_from_chain(user_id=user_id),
        "precondition": {
                "condition": lambda user_id: len(ExplorerCacheService().get_operation_chain(user_id=user_id)) > 1,
                "error_message": "Cannot undo operation: no operations to undo"
            }
    },
    "load_snapshot": {
        "operation_type": "universal",
        "operation_expected_arguments": ["user_id", "snapshot_id"],
        "operation_expected_result_data_type": "raw",
        # Here we wrap the utility method that assembles the dataset list.
        "handler": lambda user_id, **kwargs: operation_util.assemble_dataset_list_from_operation_chain(user_id, operation_chain=kwargs.get("data_source")),
        "cache_policy": lambda op: False,
        "data_source": lambda user_id, op: SnapshotsService().get_snapshot_operation_chain(
            user_id=user_id,
            snapshot_id=op.operation_arguments["snapshot_id"]
        ),
        "setup": lambda user_id, op: ExplorerCacheService().empty_operation_chain(user_id=user_id)
    },
    "group_aggregate": {
        "operation_type": "universal",
        "operation_expected_arguments": ["user_id", "group_columns", "aggregate_operation", "target_column"],
        "operation_expected_result_data_type": "enriched",
        "handler": lambda user_id, **kwargs: UniversalEnrichedService().group_aggregate(user_id=user_id, **kwargs),
        "cache_policy": lambda op: True,
        "data_source": lambda user_id, op: ExplorerCacheService().get_most_recent_operation_chain_raw_data_result(user_id=user_id),
        "setup": None
    },
    "get_unique_column_values": {
        "operation_type": "universal",
        "operation_expected_arguments": ["user_id", "column_name", "options_call"],
        "operation_expected_result_data_type": "list",
        "handler": lambda user_id, **kwargs: UniversalListService().get_unique_column_values(user_id=user_id, **kwargs),
        "cache_policy": lambda op: not op.operation_arguments.get("options_call", False),
        "data_source": lambda user_id, op: ExplorerCacheService().get_most_recent_operation_chain_raw_data_result(user_id=user_id),
        "setup": None
    },
    "get_unique_json_keys": {
        "operation_type": "universal",
        "operation_expected_arguments": ["user_id", "options_call"],
        "operation_expected_result_data_type": "list",
        "handler": lambda user_id, **kwargs: UniversalListService().get_unique_json_keys(user_id=user_id, **kwargs),
        "cache_policy": lambda op: not op.operation_arguments.get("options_call", False),
        "data_source": lambda user_id, op: ExplorerCacheService().get_most_recent_operation_chain_raw_data_result(user_id=user_id),
        "setup": None
    },
    "get_unique_json_values": {
        "operation_type": "universal",
        "operation_expected_arguments": ["user_id", "options_call"],
        "operation_expected_result_data_type": "list",
        "handler": lambda user_id, **kwargs: UniversalListService().get_unique_json_values(user_id=user_id, **kwargs),
        "cache_policy": lambda op: not op.operation_arguments.get("options_call", False),
        "data_source": lambda user_id, op: ExplorerCacheService().get_most_recent_operation_chain_raw_data_result(user_id=user_id),
        "setup": None
    },
    "get_unique_json_key_values": {
        "operation_type": "universal",
        "operation_expected_arguments": ["user_id", "json_key", "options_call"],
        "operation_expected_result_data_type": "list",
        "handler": lambda user_id, **kwargs: UniversalListService().get_unique_json_key_values(user_id=user_id, **kwargs),
        "cache_policy": lambda op: not op.operation_arguments.get("options_call", False),
        "data_source": lambda user_id, op: ExplorerCacheService().get_most_recent_operation_chain_raw_data_result(user_id=user_id),
        "setup": None
    },
    "get_count": {
        "operation_type": "universal",
        "operation_expected_arguments": ["user_id", "column_name"],
        "operation_expected_result_data_type": "metric",
        "handler": lambda user_id, **kwargs: UniversalMetricService().get_count(user_id=user_id, **kwargs),
        "cache_policy": lambda op: True,
        "data_source": lambda user_id, op: ExplorerCacheService().get_most_recent_operation_chain_raw_data_result(user_id=user_id),
        "setup": None
    },
    "get_average": {
        "operation_type": "universal",
        "operation_expected_arguments": ["user_id", "column_name"],
        "operation_expected_result_data_type": "metric",
        "handler": lambda user_id, **kwargs: UniversalMetricService().get_average(user_id=user_id, **kwargs),
        "cache_policy": lambda op: True,
        "data_source": lambda user_id, op: ExplorerCacheService().get_most_recent_operation_chain_raw_data_result(user_id=user_id),
        "setup": None
    },
    "get_sum": {
        "operation_type": "universal",
        "operation_expected_arguments": ["user_id", "column_name"],
        "operation_expected_result_data_type": "metric",
        "handler": lambda user_id, **kwargs: UniversalMetricService().get_sum(user_id=user_id, **kwargs),
        "cache_policy": lambda op: True,
        "data_source": lambda user_id, op: ExplorerCacheService().get_most_recent_operation_chain_raw_data_result(user_id=user_id),
        "setup": None
    },
    "get_min": {
        "operation_type": "universal",
        "operation_expected_arguments": ["user_id", "column_name"],
        "operation_expected_result_data_type": "metric",
        "handler": lambda user_id, **kwargs: UniversalMetricService().get_min(user_id=user_id, **kwargs),
        "cache_policy": lambda op: True,
        "data_source": lambda user_id, op: ExplorerCacheService().get_most_recent_operation_chain_raw_data_result(user_id=user_id),
        "setup": None
    },
    "get_max": {
        "operation_type": "universal",
        "operation_expected_arguments": ["user_id", "column_name"],
        "operation_expected_result_data_type": "metric",
        "handler": lambda user_id, **kwargs: UniversalMetricService().get_max(user_id=user_id, **kwargs),
        "cache_policy": lambda op: True,
        "data_source": lambda user_id, op: ExplorerCacheService().get_most_recent_operation_chain_raw_data_result(user_id=user_id),
        "setup": None
    },

    # State Operations
    "start_explorer_session": {
        "operation_type": "state",
        "operation_expected_arguments": ["user_id"],
        "operation_expected_result_data_type": "status",
        "handler": lambda user_id, **kwargs: ExplorerCacheService().create_empty_operation_chain_cache(user_id=user_id, **kwargs),
        "cache_policy": lambda op: False,
        "data_source": lambda user_id, op: None,
        "setup": None
    },
    "end_explorer_session": {
        "operation_type": "state",
        "operation_expected_arguments": ["user_id"],
        "operation_expected_result_data_type": "status",
        "handler": lambda user_id, **kwargs: ExplorerCacheService().delete_operation_chain_cache(user_id=user_id, **kwargs),
        "cache_policy": lambda op: False,
        "data_source": lambda user_id, op: None,
        "setup": None
    },
    "get_operation_chain": {
        "operation_type": "state",
        "operation_expected_arguments": ["user_id"],
        "operation_expected_result_data_type": "operation_chain",
        "handler": lambda user_id, **kwargs: ExplorerCacheService().get_operation_chain(user_id=user_id, **kwargs),
        "cache_policy": lambda op: False,
        "data_source": lambda user_id, op: None,
        "setup": None
    },
    "get_operation_chain_operations": {
        "operation_type": "state",
        "operation_expected_arguments": ["user_id"],
        "operation_expected_result_data_type": "operations_list",
        "handler": lambda user_id, **kwargs: ExplorerCacheService().extract_operation_chain_operations(user_id=user_id, **kwargs),
        "cache_policy": lambda op: False,
        "data_source": lambda user_id, op: None,
        "setup": None
    },
    "get_operation_chain_results": {
        "operation_type": "state",
        "operation_expected_arguments": ["user_id"],
        "operation_expected_result_data_type": "results_list",
        "handler": lambda user_id, **kwargs: ExplorerCacheService().extract_operation_chain_result_data(user_id=user_id, **kwargs),
        "cache_policy": lambda op: False,
        "data_source": lambda user_id, op: None,
        "setup": None
    },

    # Snapshot Operations
    "create_snapshot": {
        "operation_type": "snapshot",
        "operation_expected_arguments": ["user_id", "title"],
        "operation_expected_result_data_type": "snapshot",
        "handler": lambda user_id, data_source, **kwargs: SnapshotsService().create_snapshot(user_id=user_id, operation_chain=data_source, **kwargs),
        "cache_policy": lambda op: False,
        "data_source": lambda user_id, op: ExplorerCacheService().extract_operation_chain_operations(user_id=user_id),
        "setup": None
    },
    "delete_snapshot": {
        "operation_type": "snapshot",
        "operation_expected_arguments": ["user_id", "snapshot_id"],
        "operation_expected_result_data_type": "status",
        "handler": lambda user_id, **kwargs: SnapshotsService().delete_snapshot(user_id=user_id, **kwargs),
        "cache_policy": lambda op: False,
        "data_source": lambda user_id, op: None,
        "setup": None
    },
    "update_snapshot": {
        "operation_type": "snapshot",
        "operation_expected_arguments": ["user_id", "snapshot_id"],
        "operation_expected_result_data_type": "snapshot",
        "handler": lambda user_id, **kwargs: SnapshotsService().update_snapshot(user_id=user_id, **kwargs),
        "cache_policy": lambda op: False,
        "data_source": lambda user_id, op: None,
        "setup": None
    },
    "get_snapshot": {
        "operation_type": "snapshot",
        "operation_expected_arguments": ["user_id", "snapshot_id"],
        "operation_expected_result_data_type": "snapshot",
        "handler": lambda user_id, **kwargs: SnapshotsService().get_snapshot(user_id=user_id, **kwargs),
        "cache_policy": lambda op: False,
        "data_source": lambda user_id, op: None,
        "setup": None
    },
    "get_all_snapshots": {
        "operation_type": "snapshot",
        "operation_expected_arguments": ["user_id"],
        "operation_expected_result_data_type": "snapshot_list",
        "handler": lambda user_id, **kwargs: SnapshotsService().get_all_snapshots(user_id=user_id, **kwargs),
        "cache_policy": lambda op: False,
        "data_source": lambda user_id, op: None,
        "setup": None
    }
}
