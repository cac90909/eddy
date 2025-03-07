const OPERATION_REQUEST_DEFINITIONS = {
  "init_user": {
    "operation_expected_arguments": [],
    "http_method": "GET"
  },
  "reset": {
    "operation_expected_arguments": [],
    "http_method": "GET"
  },
  "filter": {
    "operation_expected_arguments": ["column_name", "filter_value", "filter_type"],
    "http_method": "GET"
  },
  "traverse": {
    "operation_expected_arguments": ["start_id", "traversal_directions"],
    "http_method": "GET"
  },
  "undo": {
    "operation_expected_arguments": [],
    "http_method": "POST"
  },
  "load_snapshot": {
    "operation_expected_arguments": ["snapshot_id"],
    "http_method": "GET"
  },
  "group_aggregate": {
    "operation_expected_arguments": ["group_columns", "aggregate_operation", "target_column"],
    "http_method": "GET"
  },
  "get_unique_column_values": {
    "operation_expected_arguments": ["column_name"],
    "http_method": "GET"
  },
  "get_unique_json_keys": {
    "operation_expected_arguments": [],
    "http_method": "GET"
  },
  "get_unique_json_values": {
    "operation_expected_arguments": [],
    "http_method": "GET"
  },
  "get_unique_json_key_values": {
    "operation_expected_arguments": ["json_key"],
    "http_method": "GET"
  },
  "get_unique_column_values_filter_options": {
    "operation_expected_arguments": ["column_name"],
    "http_method": "GET"
  },
  "get_unique_json_keys_filter_options": {
    "operation_expected_arguments": [],
    "http_method": "GET"
  },
  "get_unique_json_values_filter_options": {
    "operation_expected_arguments": [],
    "http_method": "GET"
  },
  "get_unique_json_key_values_filter_options": {
    "operation_expected_arguments": ["json_key"],
    "http_method": "GET"
  },
  "get_count": {
    "operation_expected_arguments": ["column_name"],
    "http_method": "GET"
  },
  "get_average": {
    "operation_expected_arguments": ["column_name"],
    "http_method": "GET"
  },
  "get_sum": {
    "operation_expected_arguments": ["column_name"],
    "http_method": "GET"
  },
  "get_min": {
    "operation_expected_arguments": ["column_name"],
    "http_method": "GET"
  },
  "get_max": {
    "operation_expected_arguments": ["column_name"],
    "http_method": "GET"
  },
  "start_explorer_session": {
    "operation_expected_arguments": [],
    "http_method": "GET"
  },
  "end_explorer_session": {
    "operation_expected_arguments": [],
    "http_method": "POST"
  },
  "get_operation_chain": {
    "operation_expected_arguments": [],
    "http_method": "GET"
  },
  "get_operation_chain_operations": {
    "operation_expected_arguments": [],
    "http_method": "GET"
  },
  "get_operation_chain_results": {
    "operation_expected_arguments": [],
    "http_method": "GET"
  },
  "create_snapshot": {
    "operation_expected_arguments": ["title"],
    "http_method": "POST"
  },
  "delete_snapshot": {
    "operation_expected_arguments": ["snapshot_id"],
    "http_method": "DELETE"
  },
  "update_snapshot": {
    "operation_expected_arguments": ["snapshot_id"],
    "http_method": "PUT"
  },
  "get_snapshot": {
    "operation_expected_arguments": ["snapshot_id"],
    "http_method": "GET"
  },
  "get_all_snapshots": {
    "operation_expected_arguments": [],
    "http_method": "GET"
  }
};
