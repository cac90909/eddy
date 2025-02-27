from shared.logger import debug_print, debug_print_vars
from shared.util import log_vars_vals_cls, catch_exceptions_cls
from explorer.services.explorer_service_config import OPERATION_DEFINITIONS
from explorer.services.explorer_cache_service import ExplorerCacheService
from explorer.services import explorer_service_util
from explorer.domain.operation import Operation

#@log_vars_vals_cls()
@catch_exceptions_cls(exception_return_value="Error")
class ExplorerService:
    def __init__(self):
        super().__init__()
        self.explorer_cache_service = ExplorerCacheService()

    def handle_operation(self, user_id, operation_name, operation_arguments):
        if operation_name not in OPERATION_DEFINITIONS:
            raise Exception(f"Operation {operation_name} not found")
        op_def = OPERATION_DEFINITIONS[operation_name]
        operation = Operation(operation_name=operation_name, operation_arguments=operation_arguments, operation_type=op_def["operation_type"])
        operation.validate_args(expected_arguments=op_def["operation_expected_arguments"])
        
        setup_method = op_def.get("setup")
        if setup_method:
            setup_method(user_id, operation)
        data_source_method = OPERATION_DEFINITIONS[operation.operation_name]["data_source"]
        data_source = data_source_method(user_id, operation)
        operation_handler = op_def["handler"]
        operation.log_info()
        operation.execute(user_id=user_id, handler=operation_handler, data_source=data_source)

        operation.validate_result(expected_result_data_type=op_def["operation_expected_result_data_type"])
        cache_policy = op_def.get("cache_policy") 
        should_cache = cache_policy(operation) if callable(cache_policy) else cache_policy
        if should_cache:
            self.explorer_cache_service.cache_operation_onto_chain(user_id=user_id, operation=operation)

        explorer_service_util.attach_data_overview(operation=operation)
        explorer_service_util.prepare_operation_result(operation=operation)
        return operation.result
    


        