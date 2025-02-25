from shared.logger import debug_print, debug_print_vars
from shared.util import log_vars_vals_cls, catch_exceptions_cls

from explorer_service_util import ( 
                                   attach_operation_data_source, 
                                   handle_operation_setup, 
                                   create_operation_instance,
                                   attach_operation_handler,
                                   validate_operation,
                                   execute_operation,
                                   handle_operation_caching,
                                   prepare_operation_result,
                                   attach_data_overview)



#@log_vars_vals_cls()
@catch_exceptions_cls(exception_return_value="Error")
class ExplorerService:
    def __init__(self):
        super().__init__()

    def handle_operation(self, user_id, operation_name, operation_arguments):
        operation = create_operation_instance(operation_name=operation_name, operation_arguments=operation_arguments)
        validate_operation(operation=operation)
        attach_operation_handler(operation=operation)
        attach_operation_data_source(user_id=user_id, operation=operation)
        handle_operation_setup(user_id=user_id, operation=operation)
        execute_operation(user_id=user_id, operation=operation)
        attach_data_overview(operation=operation)
        handle_operation_caching(user_id=user_id, operation=operation)
        prepare_operation_result(operation=operation)
        return operation["result"]


        