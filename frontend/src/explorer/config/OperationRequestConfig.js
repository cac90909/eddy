const OPERATION_REQUEST_DEFINITIONS = {
        "init_user": {
          "operation_expected_arguments": []
        },
        "reset": {
          "operation_expected_arguments": []
        },
        "filter": {
          "operation_expected_arguments": ["column_name", "filter_value", "filter_type"]
        },
        "traverse": {
          "operation_expected_arguments": ["start_id", "traversal_directions"]
        },
        "undo": {
          "operation_expected_arguments": []
        },
        "load_snapshot": {
          "operation_expected_arguments": ["snapshot_id"]
        },
        "group_aggregate": {
          "operation_expected_arguments": ["group_columns", "aggregate_operation", "target_column"]
        },
        "get_unique_column_values": {
          "operation_expected_arguments": ["column_name"]
        },
        "get_unique_json_keys": {
          "operation_expected_arguments": []
        },
        "get_unique_json_values": {
          "operation_expected_arguments": []
        },
        "get_unique_json_key_values": {
          "operation_expected_arguments": ["json_key"]
        },
        "get_unique_column_values_filter_options": {
          "operation_expected_arguments": ["column_name"]
        },
        "get_unique_json_keys_filter_options": {
          "operation_expected_arguments": []
        },
        "get_unique_json_values_filter_options": {
          "operation_expected_arguments": []
        },
        "get_unique_json_key_values_filter_options": {
          "operation_expected_arguments": ["json_key"]
        },
        "get_count": {
          "operation_expected_arguments": ["column_name"]
        },
        "get_average": {
          "operation_expected_arguments": ["column_name"]
        },
        "get_sum": {
          "operation_expected_arguments": ["column_name"]
        },
        "get_min": {
          "operation_expected_arguments": ["column_name"]
        },
        "get_max": {
          "operation_expected_arguments": ["column_name"]
        },
        "start_explorer_session": {
          "operation_expected_arguments": []
        },
        "end_explorer_session": {
          "operation_expected_arguments": []
        },
        "get_operation_chain": {
          "operation_expected_arguments": []
        },
        "get_operation_chain_operations": {
          "operation_expected_arguments": []
        },
        "get_operation_chain_results": {
          "operation_expected_arguments": []
        },
        "create_snapshot": {
          "operation_expected_arguments": ["title"]
        },
        "delete_snapshot": {
          "operation_expected_arguments": ["snapshot_id"]
        },
        "update_snapshot": {
          "operation_expected_arguments": ["snapshot_id"]
        },
        "get_snapshot": {
          "operation_expected_arguments": ["snapshot_id"]
        },
        "get_all_snapshots": {
          "operation_expected_arguments": []
        }
      }      