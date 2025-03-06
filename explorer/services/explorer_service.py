from shared.logger import debug_print, debug_print_vars
from shared.util import log_vars_vals_cls, catch_exceptions_cls
from explorer.config.operation_config import OPERATION_DEFINITIONS
from explorer.services.explorer_cache_service import ExplorerCacheService
from explorer.util import operation_util
from explorer.domain.operation import Operation
from explorer.domain.operation_result import OperationResult
from explorer.util import operation_result_util
from explorer.config.operation_result_config import OPERATION_RESULT_DEFINITIONS

#@log_vars_vals_cls()
@catch_exceptions_cls(exception_return_value="Error")
class ExplorerService:
    def __init__(self):
        super().__init__()
        self.explorer_cache_service = ExplorerCacheService()   

    def handle_operation(self, user_id, operation_name, operation_arguments):
        op_def = OPERATION_DEFINITIONS[operation_name]
        #Request Validation
        if operation_name not in OPERATION_DEFINITIONS:
            debug_print(f"Operation {operation_name} not found")
            return OperationResult(data=None, meta={"data_type":"error", "error_message": f"Operation not found: {operation_name}"}).to_dict()
        for arg in op_def.get("operation_expected_arguments"):
            if arg not in operation_arguments and arg != "user_id": #Suspending check on user_id for right now
                return OperationResult(data=None, meta={"data_type":"error", "error_message": f"Missing argument {arg}"}).to_dict() 
        if op_def.get("precondition") and not op_def.get("precondition")["condition"](user_id=user_id):
            return OperationResult(data=None, meta={"data_type":"error", "error_message": f"Precondition failure: {op_def.get('precondition')['error_message']}"}).to_dict()
    
        #Initalization/Setup
        operation = Operation(operation_name=operation_name, operation_arguments=operation_arguments, operation_type=op_def["operation_type"])
        if op_def.get("setup"):
            op_def.get("setup")(user_id, operation)
        if op_def.get("data_source"):
            data_source = op_def.get("data_source")(user_id, operation)
        operation_handler = op_def["handler"]
        
        #Execution
        try:
            result_data = operation.execute(user_id=user_id, handler=operation_handler, data_source=data_source)
        except Exception as e:
            debug_print(f"Error executing operation {operation_name}: {e}")
            return OperationResult(data=None, meta={"data_type":"error", "error_message": f"Execution error: {str(result_data)}"}).to_dict()

        debug_print(result_data)
        #Result Data Validation
        op_resp_def = OPERATION_RESULT_DEFINITIONS[op_def["operation_expected_result_data_type"]]
        valid_resp_data_type = op_resp_def["data_type_verification"](result_data=result_data)
        if not valid_resp_data_type:
            return OperationResult(data=None, meta={"data_type":"error", "error_message": f"""Mismatch on Operation Result Data Type: {str(result_data)}. 
                                                    Expected Type: {op_def['operation_expected_result_data_type']}, Received Type: {type(result_data)}"""}).to_dict()
        operation.result_data = result_data
        operation.result_data_type = op_def["operation_expected_result_data_type"]

        #Caching
        op_cache_policy, op_resp_cache_policy = op_def.get("cache_policy")(operation), op_resp_def.get("cache_policy")(operation)
        if op_cache_policy and op_resp_cache_policy:
            self.explorer_cache_service.cache_operation_onto_chain(user_id=user_id, operation=operation)
        operation_util.cache_operation_metadata(user_id=user_id, operation=operation)
        
        #Response Creation
        operation_result = OperationResult(data=operation.result_data, meta={"data_type": operation.result_data_type, "operation_bid": operation.id})
        data_overview = {}
        for data_overview_field_name, data_overview_field_calc in op_resp_def.get("data_overview_fields").items():
            data_overview[data_overview_field_name] = data_overview_field_calc(user_id=user_id, result_data=operation_result.data)
        if data_overview:
            operation_result.meta["data_overview"] = data_overview 
        if op_resp_def.get("operations_history"):
            operation_result.meta["operations_history"] = op_resp_def.get("operations_history")(user_id)
        return operation_result.to_dict()

        
    


        