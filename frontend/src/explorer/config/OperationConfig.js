export const OPERATION_CONFIG = {
    // Universal operations
    get_full_data: {
      op_name:    "get_full_data",
      op_args:    [],
      httpMethod: "GET",
    },
    filter: {
      op_name:    "filter",
      op_args:    ["column_name", "filter_value", "filter_type"],
      httpMethod: "GET",
    },
    traverse: {
      op_name:    "traverse",
      op_args:    ["start_id", "traversal_directions"],
      httpMethod: "GET",
    },
    group_aggregate: {
      op_name:    "group_aggregate",
      op_args:    ["group_columns", "aggregate_operation", "target_column", "frequency"],
      httpMethod: "GET",
    },
    get_unique_column_values: {
      op_name:    "get_unique_column_values",
      op_args:    ["column_name"],
      httpMethod: "GET",
    },
    get_unique_json_keys: {
      op_name:    "get_unique_json_keys",
      op_args:    [],
      httpMethod: "GET",
    },
    get_unique_json_values: {
      op_name:    "get_unique_json_values",
      op_args:    [],
      httpMethod: "GET",
    },
    get_unique_json_key_values: {
      op_name:    "get_unique_json_key_values",
      op_args:    ["json_key"],
      httpMethod: "GET",
    },
  
    get_unique_column_values_filter_options: {
      op_name:    "get_unique_column_values_filter_options",
      op_args:    ["column_name"],
      httpMethod: "GET",
    },
    get_unique_json_keys_filter_options: {
      op_name:    "get_unique_json_keys_filter_options",
      op_args:    [],
      httpMethod: "GET",
    },
    get_unique_json_values_filter_options: {
      op_name:    "get_unique_json_values_filter_options",
      op_args:    [],
      httpMethod: "GET",
    },
    get_unique_json_key_values_filter_options: {
      op_name:    "get_unique_json_key_values_filter_options",
      op_args:    ["json_key"],
      httpMethod: "GET",
    },
  
    get_count: {
      op_name:    "get_count",
      op_args:    ["column_name"],
      httpMethod: "GET",
    },
    get_average: {
      op_name:    "get_average",
      op_args:    ["column_name"],
      httpMethod: "GET",
    },
    get_sum: {
      op_name:    "get_sum",
      op_args:    ["column_name"],
      httpMethod: "GET",
    },
    get_min: {
      op_name:    "get_min",
      op_args:    ["column_name"],
      httpMethod: "GET",
    },
    get_max: {
      op_name:    "get_max",
      op_args:    ["column_name"],
      httpMethod: "GET",
    },
  
    // State operations
    start_explorer_session: {
      op_name:    "start_explorer_session",
      op_args:    [],
      httpMethod: "GET",
    },
    reset_explorer_session: {
      op_name:    "reset_explorer_session",
      op_args:    [],
      httpMethod: "GET",
    },
    undo_operation: {
      op_name:    "undo_operation",
      op_args:    [],
      httpMethod: "POST",
    },
    load_snapshot: {
      op_name:    "load_snapshot",
      op_args:    ["snapshot_id"],
      httpMethod: "GET",
    },
    end_explorer_session: {
      op_name:    "end_explorer_session",
      op_args:    [],
      httpMethod: "POST",
    },
  
    // Snapshot operations
    save_snapshot: {
      op_name:    "save_snapshot",
      op_args:    ["title", "description"],
      httpMethod: "POST",
    },
    delete_snapshot: {
      op_name:    "delete_snapshot",
      op_args:    ["snapshot_id"],
      httpMethod: "DELETE",
    },
    update_snapshot: {
      op_name:    "update_snapshot",
      op_args:    ["snapshot_id", "title", "description"],
      httpMethod: "PUT",
    },
    get_snapshot: {
      op_name:    "get_snapshot",
      op_args:    ["snapshot_id"],
      httpMethod: "GET",
    },
    get_all_snapshots: {
      op_name:    "get_all_snapshots",
      op_args:    [],
      httpMethod: "GET",
    },
  
    // Utility / operations chain
    get_operation_chain: {
      op_name:    "get_operation_chain",
      op_args:    [],
      httpMethod: "GET",
    },
    get_operation_chain_operations: {
      op_name:    "get_operation_chain_operations",
      op_args:    [],
      httpMethod: "GET",
    },
    get_operation_chain_results: {
      op_name:    "get_operation_chain_results",
      op_args:    [],
      httpMethod: "GET",
    },
    get_column_operator_options: {
      op_name:    "get_column_operator_options",
      op_args:    [],
      httpMethod: "GET",
    },
    get_filterable_column_names: {
      op_name:    "get_filterable_column_names",
      op_args:    [],
      httpMethod: "GET",
    },
    get_operation_types: {
      op_name:    "get_operation_types",
      op_args:    [],
      httpMethod: "GET",
    },
    get_operation_names_for_result_data_type: {
      op_name:    "get_operation_names_for_result_data_type",
      op_args:    ["operation_result_data_type"],
      httpMethod: "GET",
    },
    get_operation_argument_names: {
      op_name:    "get_operation_argument_names",
      op_args:    ["operation_name"],
      httpMethod: "GET",
    },
    get_operation_argument_options: {
      op_name:    "get_operation_argument_options",
      op_args:    ["operation_name", "operation_argument_name", "prev_argument_values"],
      httpMethod: "GET",
    },
  };