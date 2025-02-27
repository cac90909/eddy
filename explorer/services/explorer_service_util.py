from shared.logger import debug_print

@staticmethod
def assemble_dataset_list_from_operation_chain(user_id, operation_chain):
    from explorer.services.explorer_service import ExplorerService  # local import to avoid circular dependency
    explorer_service = ExplorerService()
    result = None
    for op in operation_chain:
        # Assume each operation in the chain is a dict with keys "operation_name" and "operation_arguments"
        op_name = op.get("operation_name")
        op_args = op.get("operation_arguments")
        try:
            result = explorer_service.handle_operation(user_id, op_name, op_args)
        except Exception as e:
            debug_print(f"Error executing operation {op_name}: {e}")
            return
    return result
        
@staticmethod
def attach_data_overview(operation):
    try:
        operation_type, operation_result_data_type, operation_result_data = operation.operation_type, operation.result["data_type"], operation.result["data"]
        if operation_type == "universal":
            if operation_result_data_type in ["raw", "enriched"]:
                print(operation.result["data"])
                print(operation.to_dict())
                num_rows = len(operation_result_data)
                # Try to get the number of fields from the first instance's _meta (if available)
                try:
                    num_columns = len(operation_result_data[0]._meta.fields)
                except AttributeError:
                    # If it's not a Django model instance, fallback (NOTE: this would apply to enriched):
                    num_columns = len(operation_result_data[0]) if isinstance(operation_result_data[0], (dict, list)) else 1
                    num_values = num_rows * num_columns
                else:
                    num_rows = num_columns = num_values = 0
                operation.result["data_overview"] = {
                    "num_rows": num_rows,
                    "num_columns": num_columns,
                    "num_values": num_values
                }
                # operation.result.data_overview = {
                #     "num_rows": operation.result["data"].count(),
                #     "num_columns" : len(operation.result["data"].count()),
                #     "num_values" : operation.result["data"].count() * len(operation.result["data"].count())
                # }
            if operation_result_data_type == "list":
                operation.result.data_overview = {
                    "num_values": len(operation.result["data"])
                }
            if operation_result_data_type == "metric":
                operation.result.data_overview = {
                    "num_values": 1
                }
            debug_print("Data Overview Attached")
        else:
            pass
        return True
    except Exception as e:
        debug_print(f"Error attaching data overview: {e}")
        return False
        
@staticmethod
def prepare_operation_result(operation):
    operation_result = operation.result
    operation_result_data, operation_result_data_type = operation_result["data"], operation_result["data_type"]
    if operation_result_data_type in ["raw", "enriched", "snapshot_list"]:
        operation.result["data"] = list(operation.result["data"])
    if operation_result_data_type in ["list", "metric", "status", "snapshot",  "operations_list"]:
        operation.result["data"] = operation.result["data"]
    if operation_result_data_type == "operation_chain":
        operation_chain = operation_result_data
        for sub_operation in operation_chain:
            sub_operation_dict = operation.to_dict()
            sub_operation_result_data_type = sub_operation_dict["result"]["data_type"]
            if sub_operation_result_data_type in ["raw", "enriched"]:
                sub_operation_dict["data"] = list(sub_operation_dict["data"])
            if sub_operation_result_data_type in ["list", "metric"]:
                sub_operation_dict["data"] = sub_operation_dict["data"]
    if operation_result_data_type == "results_list":
        results_list = operation_result
        for result in results_list:
            result_data_type = result["data_type"]
            if result_data_type in ["raw", "enriched"]:
                result["data"] = list(result["data"])
            if result_data_type in ["list", "metric"]:
                result["data"] = result["data"]
    debug_print("Operation Result Prepared")
    return True






    
