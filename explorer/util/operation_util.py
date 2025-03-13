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
#TODO - change this to only send operation names as they pertain to binding them to UI components
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

#Eventually, these will be DB queries (operation names, argument definitions I think will eventually be stored in DB)
#TODO - change the naming scope (and list sontruction scope) for only operations that pertain to the operation request constructor
def get_operation_result_data_types(user_id):
    from explorer.config.operation_config import OPERATION_DEFINITIONS
    operation_result_data_types = [op_def.get("operation_result_data_type") for op_def in OPERATION_DEFINITIONS.values()]
    return set(operation_result_data_types)

def get_operation_names_for_result_data_type(user_id, operation_result_data_type):
    from explorer.config.operation_config import OPERATION_DEFINITIONS
    operation_names = [op_def.get("operation_name") for op_def in OPERATION_DEFINITIONS.values() if op_def.get("operation_expected_result_data_type") == operation_result_data_type]
    return operation_names


def get_operation_argument_names(user_id, operation_name):
    from explorer.config.operation_config import OPERATION_DEFINITIONS
    op_def = next((op for op in OPERATION_DEFINITIONS if op["operation_name"] == operation_name), None)
    if op_def:
        return [arg.get("argument_name") for arg in op_def.get("operation_expected_arguments") if arg.get("argument_name") != "user_id"]
    else:
        return None
    
def get_operation_argument_options(user_id, operation_name, operation_argument_name, prev_argument_values):
    from explorer.config.operation_config import OPERATION_DEFINITIONS
    op_def = next((op for op in OPERATION_DEFINITIONS if op["operation_name"] == operation_name), None)
    arg_def = next((arg for arg in op_def.get("operation_expected_arguments") if arg.get("argument_name") == operation_argument_name), None)
    if arg_def.get("value_options"):
        return arg_def.get("value_options")(user_id, prev_argument_values)
    elif arg_def.get("value_options_fetch"): 
        from explorer.services.explorer_service import ExplorerService
        if arg_def.get("value_options_dependency") is None:
            return ExplorerService().handle_operation(user_id=user_id, operation_name=arg_def.get("value_options_fetch"), operation_arguments={})
        elif "$" in arg_def.get("value_options_dependency"):
            dep_arg = prev_argument_values[arg_def.get("value_options_dependency")[1:]]
            return ExplorerService(user_id=user_id, operation_name=arg_def.get("value_options_dependency"), operation_arguments={arg_def.get("value_options_dependency")[1:]:prev_argument_values[dep_arg]})
        elif "$" not in arg_def.get("value_options_dependency"):
            return ExplorerService(user_id=user_id, operation_name=arg_def.get("value_options_dependency"), operation_arguments={arg_def.get("value_options_dependency"):prev_argument_values[arg_def.get("value_options_dependency")]}) 
    else:
        return None
        

    






    
