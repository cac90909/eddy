from django.core.cache import cache
from shared.services.cache_service import CacheService
from shared.logger import debug_print_vars, debug_print
from shared.util import log_vars_vals_cls, catch_exceptions_cls



@catch_exceptions_cls(exception_return_value={"success": False})
class ExplorerCacheService(CacheService):
    MAX_CACHE_LEN = 50  # Max items in a specific cache object for a user

    # Named variables for subcache keys
    OPERAION_CHAIN_KEY = "OPERATION_CHAIN"

    # ---- Initialization & Cache Setup Methods ----
    def create_empty_operation_chain_cache(self, user_id):
        if not self.get_user_cache(user_id=user_id):
            self.create_empty_user_cache(user_id=user_id)
        self.cache_user_obj(user_id=user_id, obj_key=self.OPERAION_CHAIN_KEY, obj_val=[])
        return "Success"

    def empty_operation_chain(self, user_id):
        self.cache_user_obj(user_id=user_id, obj_key=self.OPERAION_CHAIN_KEY, obj_val=[])
        return "Success"

    # ---- Retrieval Methods ----
    def get_operation_chain(self, user_id):
        operation_chain = self.get_user_cache_obj(user_id=user_id, obj_key=self.OPERAION_CHAIN_KEY)
        return operation_chain 

    def get_most_recent_operation_from_chain(self, user_id):
        operation_chain = self.get_operation_chain(user_id=user_id)
        most_recent_operation = operation_chain[-1] if operation_chain else None
        return most_recent_operation

    def get_most_recent_operation_chain_result(self, user_id):
        operation_chain_results = self.extract_operation_chain_result_data(user_id=user_id)
        most_recent_data_result = operation_chain_results[-1] if operation_chain_results else None
        return most_recent_data_result

    def get_most_recent_operation_chain_result_data(self, user_id):
        most_recent_result = self.get_most_recent_operation_chain_result(user_id=user_id)
        return most_recent_result["data"] if most_recent_result else None

    def get_most_recent_operation_chain_raw_data_result(self, user_id):
        operation_chain = self.get_operation_chain(user_id=user_id)
        for operation in operation_chain[::-1]:
            if operation.result["data_type"] == "raw":
                return operation.result["data"]
        return None

    # ---- Modification Methods (Adding/Removing Operations) ----
    def cache_operation_onto_chain(self, user_id, operation):
        operation_chain = self.get_operation_chain(user_id=user_id)
        operation_chain.append(operation)
        self.cache_operation_chain(user_id=user_id, operation_chain=operation_chain)
        return "Success"

    def cache_operation_chain(self, user_id, operation_chain):
        self.cache_user_obj(user_id=user_id, obj_key=self.OPERAION_CHAIN_KEY, obj_val=operation_chain)
        return "Success"

    def delete_operation_chain_cache(self, user_id):
        self.delete_user_cache_obj(user_id=user_id, obj_key=self.OPERAION_CHAIN_KEY)
        return "Success"

    def delete_most_recent_operation_from_chain(self, user_id):
        operation_chain = self.get_operation_chain(user_id=user_id)
        operation_chain.pop(-1)
        self.cache_operation_chain(user_id=user_id, operation_chain=operation_chain)
        return "Success"

    # ---- Helper/Extraction Methods ----

    def extract_operation_chain_operations(self, user_id):
        operation_chain = self.get_operation_chain(user_id=user_id)
        operation_chain_operations = []
        for operation in operation_chain:
            operation_chain_operations.append({
                "operation_name": operation.operation_name,
                "operation_arguments": operation.operation_arguments,
                "operation_type": operation.operation_type
            })
        return operation_chain_operations

    def extract_operation_chain_result_data(self, user_id):
        operation_chain = self.get_operation_chain(user_id=user_id)
        operation_chain_operations = []
        for operation in operation_chain:
            operation_chain_operations.append(operation.result)
        return operation_chain_operations
    
