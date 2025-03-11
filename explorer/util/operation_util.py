from shared.logger import debug_print
from shared.models import Universal
from explorer.services.explorer_cache_service import ExplorerCacheService


def get_operation_definition(operation_name):
    from explorer.config.operation_config import OPERATION_DEFINITIONS
    return next((op for op in OPERATION_DEFINITIONS if op["operation_name"] == operation_name), None)

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

@staticmethod
def assemble_operations_config(user_id):
    from explorer.config.operation_config import OPERATION_DEFINITIONS
    operations_config = [{"operation_name":op_def.get("operation_name"), "operation_arguments": op_def.get("operation_expected_arguments"),
                           "operation_result_data_type": op_def.get("operation_expected_result_data_type"), "http_method": op_def.get("http_method"),
                           "display": op_def.get("display")} 
                          for op_name, op_def in OPERATION_DEFINITIONS.items()]

    
    return operations_config



def get_column_operator_options(user_id, data_source, column_name):
    from shared.repositories import universal_repository_util
    column_data_type = universal_repository_util.get_data_type_from_column_identifier(queryset=data_source, column_name=column_name)
    operators = universal_repository_util.get_operators_from_data_type_identifier(data_type_identifier=column_data_type)
    return operators



def get_filterable_column_names(user_id, data_source):
    from explorer.util import operation_result_util
    from shared.services.universal_list_service import UniversalListService
    model_columns_list = ["functionalities", "subject_matters", "general_categories", "tags", "entry_id"]
    if operation_result_util.is_queryset_of_model_instances(data_source, Universal):
        keys_list = UniversalListService().get_unique_json_keys(user_id=user_id, data_source=data_source)
        merged_list = model_columns_list + keys_list 
        return merged_list
    else:
        debug_print(f"Data source is not a queryset of model instances")
        return None

    






    
