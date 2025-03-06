from shared.logger import debug_print
from explorer.services.explorer_cache_service import ExplorerCacheService


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
def cache_operation_metadata(user_id, operation):
    explorer_cache_service = ExplorerCacheService()
    operation_chain = explorer_cache_service.get_operation_chain(user_id=user_id)
    if operation.result_data_type == "raw":
        if len(operation_chain) == 1:
            full_data_shape = {
                "num_rows": len(operation.result_data),
                "num_columns": len(operation.result_data[0]._meta.fields),
                "num_values" : len(operation.result_data) * len(operation.result_data[0]._meta.fields)
            }
            explorer_cache_service.cache_operation_chain_metadata(user_id=user_id, metadata_key="full_data_shape", metadata_value=full_data_shape)
    #Only update chain number if previous operation was raw (since right now, only raw operations overwrite the underlying data)
    if operation_chain and len(operation_chain) > 1 and operation_chain[-2].result_data_type == "raw":
        explorer_cache_service.cache_operation_chain_metadata(user_id=user_id, metadata_key="operations_on_chain", metadata_value=len(operation_chain))
    debug_print(f"Operation metadata cached")

    






    
