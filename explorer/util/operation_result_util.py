from django.db.models import Model
from django.db.models.query import QuerySet
from explorer.domain.operation_result import OperationResult
from explorer.domain.operation import Operation
from shared.models import Universal
from shared.logger import debug_print


# ---------- Type Checking ----------
def is_model_instance(result_data, model_class):
    return isinstance(result_data, model_class) 

def is_model_instance_list(result_data, model_class):
    if not isinstance(result_data, list):
        return False
    return all([is_model_instance(v, model_class) for v in result_data])

def is_list_of_model_instance_lists(result_data, model_class):
    if not isinstance(result_data, list):
        return False
    return all([is_model_instance_list(v, model_class) for v in result_data])

def is_queryset_of_model_instances(result_data, model_class):
    if not isinstance(result_data, QuerySet):
        return False
    return all(isinstance(item, model_class) for item in result_data)

def is_list_of_querysets_of_model_instances(result_data, model_class):
    if not isinstance(result_data, list):
        return False
    return all(is_queryset_of_model_instances(q, model_class) for q in result_data)

def is_queryset_of_dicts(result_data):
    if not isinstance(result_data, QuerySet):
        return False
    return all(isinstance(item, dict) for item in result_data)

def is_list_of_dicts(result_data):
    if not isinstance(result_data, list):
        return False
    return all(isinstance(item, dict) for item in result_data)

def is_list_of_operation_instances(result_data):
    if not isinstance(result_data, list):
        return False
    return all(isinstance(item, Operation) for item in result_data)

def create_error_operation_result(error_message):
    return OperationResult(data=None, meta= {"error_message": error_message})

def create_status_operation_result(status_message):
    return OperationResult(data=None, meta= {"error_message": status_message})

# ---------- Serialization ----------

def serialize_results_list(results_list):
    from explorer.config.operation_result_config import OPERATION_RESULT_DEFINITIONS
    serialized_results = []
    for result in results_list:
        if is_queryset_of_model_instances(result, Universal):
            op_res_def = OPERATION_RESULT_DEFINITIONS["raw"]
            serialized_res = op_res_def["serialization"](result)
            serialized_results.append(serialized_res)
        elif is_queryset_of_dicts(result):
            op_res_def = OPERATION_RESULT_DEFINITIONS["enriched"]
            serialized_res = op_res_def["serialization"](result)
            serialized_results.append(serialized_res)
        elif isinstance(result, list):
            op_res_def = OPERATION_RESULT_DEFINITIONS["list"]
            serialized_res = op_res_def["serialization"](result)
            serialized_results.append(serialized_res)
        elif isinstance(result, int):
            op_res_def = OPERATION_RESULT_DEFINITIONS["metric"]
            serialized_res = op_res_def["serialization"](result)
            serialized_results.append(serialized_res)
    return serialized_results

def serialize_operation_chain(operation_chain):
    from explorer.config.operation_result_config import OPERATION_RESULT_DEFINITIONS
    serialized_chain = []
    for operation in operation_chain:
        op_dict = operation.to_dict()
        op_res_def = OPERATION_RESULT_DEFINITIONS[op_dict["result_data_type"]]
        serialized_res = op_res_def["serialization"](op_dict["result_data"])
        op_dict["result_data"] = serialized_res
        serialized_chain.append(op_dict)
    return serialized_chain 

# @staticmethod
# def get_unique_value_options(user_id):
#     from shared.services.universal_list_service import UniversalListService
#     universal_list_service = UniversalListService()
#     from explorer.services.explorer_cache_service import ExplorerCacheService
#     explorer_cache_service = ExplorerCacheService()
#     most_recent_raw_data = explorer_cache_service.get_most_recent_operation_chain_raw_data_result(user_id=user_id)
#     unique_value_options = []
    
#     #TODO - combine this into a single (or a couple) queries, right now this is like 30+ queries
#     #TODO - attach datatype for each columns so the frontend knows which columns can have which operations performed on them
#     query_columns = ["functionalities", "subject_matters", "general_categories", "tags", "entry_id"]
#     for col in query_columns:
#         unique_vals = universal_list_service.get_unique_column_values(user_id=user_id, data_source=most_recent_raw_data, column_name=col)
#         unique_value_options.append({"column_name": col, "unique_values": unique_vals})
#     unique_keys = universal_list_service.get_unique_json_keys(user_id=user_id, data_source=most_recent_raw_data)
#     for json_key in unique_keys:
#         unique_vals = universal_list_service.get_unique_json_key_values(user_id=user_id, data_source=most_recent_raw_data, json_key=json_key)
#         unique_value_options.append({"column_name": json_key+ " (fields)", "unique_values": unique_vals})
    
#     return unique_value_options      

@staticmethod
def get_operation_history(user_id):
    from explorer.services.explorer_cache_service import ExplorerCacheService
    explorer_cache_service = ExplorerCacheService()
    operations_history = explorer_cache_service.extract_operation_chain_operations(user_id=user_id)
    return operations_history

@staticmethod
def get_all_snapshots(user_id):
    from shared.services.snapshots_service import SnapshotsService
    snapshot_service = SnapshotsService()
    snapshots = snapshot_service.get_all_snapshots(user_id=user_id)
    snapshots_refined = snapshots.values("snapshot_id", "title", "description")
    return list(snapshots_refined)

#TODO
#So I started going in this direction yesterday, but I want to handhold the frontend even more
#update the operation_navigation meta to include be more granular and step by step:
#operation_type: {
#    operation_name: [
#        {
#            "argument_name": "column_name",
#             "choices": "choices",
#            "display_type": "dropdown",
#            "required": True
#        },
#        {
#            "argument_name": "filter_value",
#            "choices": 
#                       ^^^^^^^^^^^^^^^^^
# Here is where it gets spicy, it would be a huge request to precaclculate all the choices
#for every single operation, after every single operation
#So two choices:
#1. sequertial operation argument choices that depend on the inital argument choice can send
#   a prefilled out request that lets the frontend know to send a follow up request to get choices
#   based on the initial choice.
#2. ORRRR, I can experiment with GraphQL, I think this would honestly be a decent use case for it


#I am leaning towards #1 for ease of implementation, but I think #2 would be good to implement in the future given
    #I am essentially constructing a decisoin tree here, so it would resemelbe grpahql natural structure