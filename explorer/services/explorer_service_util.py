from shared.logger import debug_print


@staticmethod
def assemble_dataset_list_from_operation_chain(user_id, operation_chain):
    from explorer.services.explorer_service import ExplorerService  # local import to avoid circular dependency
    explorer_service = ExplorerService()
    final_result = None
    for op in operation_chain:
        op_name = op.get("operation_name")
        op_args = op.get("operation_arguments")
        try:
            op_response = explorer_service.handle_operation(user_id, op_name, op_args)
        except Exception as e:
            debug_print(f"Error executing operation {op_name}: {e}")
            return
        # Instead of storing the whole op_response, extract its "data" key if available
        if isinstance(op_response, dict) and "data" in op_response:
            final_result = op_response["data"]
        else:
            final_result = op_response
    return final_result
        
@staticmethod
def attach_data_overview(operation):
    try:
        operation_type, operation_result_data_type, operation_result_data = operation.operation_type, operation.result["data_type"], operation.result["data"]
        num_values = None
        if operation_result_data_type == "raw":
            num_rows = len(operation_result_data)
            num_columns = len(operation_result_data[0]._meta.fields) 
            num_values = num_rows * num_columns
            operation.result["data_overview"] = {
                "num_rows": num_rows,
                "num_columns": num_columns,
                "num_values": num_values
            }
        if operation_result_data_type == "enriched":
            num_rows = len(operation_result_data)
            num_columns = len(operation_result_data[0]) 
            num_values = num_rows * num_columns
            operation.result["data_overview"] = {
                "num_rows": num_rows,
                "num_columns": num_columns,
                "num_values": num_values
            }
        if operation_result_data_type == "list":
            num_values = len(operation.result["data"])
            operation.result["data_overview"] = {
                "num_values": num_values
            }
        if operation_result_data_type == "metric":
            num_values = 1
            operation.result["data_overview"] = {
                "num_values": num_values
            }
        if operation_result_data_type == "operation_chain":
            operation_names = [op.operation_name for op in operation.result["data"]]
            data_types = [op.result["data_type"] for op in operation.result["data"]]
            num_values = len(operation.result["data"])
            operation.result["data_overview"] = {
                "num_operations": num_values,
                "operation_names": operation_names,
                "data_types": data_types
            }
        if operation_result_data_type == "results_list":
            data_types = [res["data_type"] for res in operation.result["data"]]
            num_values = len(operation.result["data"])
            operation.result["data_overview"] = {
                "num_results": num_values,
                "data_types": data_types
            }
        if operation_result_data_type == "operations_list":
            num_values = len(operation.result["data"])
            operation.result["data_overview"] = {
                "num_operations": num_values
            }
        if operation_result_data_type == "snapshot_list":
            num_values = len(operation.result["data"])
            operation.result["data_overview"] = {
                "num_snapshots": num_values
            }
        debug_print(f"{num_values} values attached")
    except Exception as e:
        debug_print(f"Error attaching data overview: {e}")
    
@staticmethod
def verify_operation_preconditions(user_id, operation):
    operation_name = operation.operation_name
    if operation_name == "undo":
        from explorer.services.explorer_cache_service import ExplorerCacheService
        explorer_cache_service = ExplorerCacheService()
        operation_chain = explorer_cache_service.get_operation_chain(user_id=user_id)
        if len(operation_chain) < 2:
            return False, "Cannot undo operation: no operations to undo"
    return True, None
        

#NOTE: Logic contained within serializer at the moment
@staticmethod
def prepare_operation_result(operation):
    try:
        operation_result = operation.result
        data = operation_result.get("data")
        data_type = operation_result.get("data_type")
        if data_type in ["raw", "enriched", "snapshot_list"]:
            operation_result["data"] = list(data)
        elif data_type in ["list", "metric", "status", "snapshot", "operations_list"]:
            operation_result["data"] = data
        elif data_type == "operation_chain":
            new_chain = []
            for sub_op in data:
                sub_op_result_data_type = sub_op.result["data_type"]
                if sub_op_result_data_type in ["raw", "enriched"]:
                    new_sub_op_result_data = list(sub_op.result["data"])
                elif sub_op_result_data_type in ["list", "metric"]:
                    new_sub_op_result_data = sub_op.result["data"]
                

                new_sub_op_dict = sub_op.to_dict()
                new_sub_op_dict_result = new_sub_op_dict["result"]
                new_sub_op_dict_result["data"] = new_sub_op_result_data
            
                new_chain.append(new_sub_op_dict)
            operation_result["data"] = new_chain
            return
        elif data_type == "results_list":
            new_results = []
            for result in data:
                result_data_type = result.get("data_type")
                if result_data_type in ["raw", "enriched"]:
                    result["data"] = list(result["data"])
                elif data_type in ["list", "metric"]:
                    result["data"] = result["data"]
                new_results.append(result)
            operation_result["data"] = new_results
    except Exception as e:
        debug_print(f"Error preparing operation result: {e}")
        return False
    debug_print(f"Operation result prepared")
    return True






    
