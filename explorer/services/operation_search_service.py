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
class OperationSearchService:

    def __init__(self):
        super().__init__()
        self.explorer_cache_service = ExplorerCacheService()  

    #Approach 1: given any string, output the appropriate info (possible values, signature, etc)
    #Approach 2: different methods for different types of 

    def handle_search(search_query):
        pass