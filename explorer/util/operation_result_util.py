from django.db.models import Model
from django.db.models.query import QuerySet
from explorer.domain.operation_result import OperationResult
from explorer.domain.operation import Operation
from shared.models import Universal
from explorer.services.explorer_cache_service import ExplorerCacheService
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


