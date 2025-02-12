import functools
from shared.logger import debug_print_vars, debug_print  # (Assuming this is now used only internally by decorators)
from shared.services.cache_service import CacheService

# Assume these class-level decorators have been defined in your shared.utilities module:
# They apply logging (log_values) and exception catching (catch_exceptions_class) to all methods.
from shared.util import log_vars_vals_cls, catch_exceptions_cls

#@log_vars_vals_cls()  # By default, this applies to all callable methods (excluding special methods)
@catch_exceptions_cls(exception_return_value={"success": False})
class ExplorerCacheService(CacheService):
    MAX_CACHE_LEN = 10  # Max items in a specific cache object for a user

    # Named variables for subcache keys
    DATASETS_CACHE_KEY = "datasets"
    OPERATION_CHAIN_CACHE_KEY = "operation_chain"

    def create_empty_explorer_cache(self, user_id):
        debug_print("Check if user already has a cache --> cache an empty datasets list and operation list")
        if not self.get_user_cache(user_id=user_id):
            self.create_empty_user_cache(user_id=user_id)
        self.cache_user_obj(user_id=user_id, obj_key=self.DATASETS_CACHE_KEY, obj_val=[])
        self.cache_user_obj(user_id=user_id, obj_key=self.OPERATION_CHAIN_CACHE_KEY, obj_val=[])

    def cache_dataset_item(self, user_id, dataset):
        debug_print()
        datasets = self.get_user_cache_obj(user_id=user_id, obj_key=self.DATASETS_CACHE_KEY)
        if len(datasets) >= self.MAX_CACHE_LEN:
            datasets.pop(0)
        datasets.append(dataset)
        self.cache_user_obj(user_id=user_id, obj_key=self.DATASETS_CACHE_KEY, obj_val=datasets)

    def cache_operation_item(self, user_id, operation):
        debug_print()
        operation_chain = self.get_user_cache_obj(user_id=user_id, obj_key=self.OPERATION_CHAIN_CACHE_KEY)
        if len(operation_chain) >= self.MAX_CACHE_LEN:
            operation_chain.pop(0)
        operation_chain.append(operation)
        self.cache_user_obj(user_id=user_id, obj_key=self.OPERATION_CHAIN_CACHE_KEY, obj_val=operation_chain)

    def cache_dataset_list(self, user_id, dataset_list):
        debug_print()
        if len(dataset_list) > self.MAX_CACHE_LEN:
            dataset_list = dataset_list[-self.MAX_CACHE_LEN:]
        self.cache_user_obj(user_id=user_id, obj_key=self.DATASETS_CACHE_KEY, obj_val=dataset_list)

    def cache_operation_chain(self, user_id, operation_chain):
        debug_print()
        if len(operation_chain) > self.MAX_CACHE_LEN:
            operation_chain = operation_chain[-self.MAX_CACHE_LEN:]
        self.cache_user_obj(user_id=user_id, obj_key=self.OPERATION_CHAIN_CACHE_KEY, obj_val=operation_chain)

    def get_most_recent_dataset(self, user_id):
        debug_print()
        datasets = self.get_user_cache_obj(user_id=user_id, obj_key=self.DATASETS_CACHE_KEY)
        most_recent_dataset = datasets[-1] if datasets else None
        return most_recent_dataset
    
    def get_most_recent_operation(self, user_id):
        debug_print()
        operation_chain = self.get_user_cache_obj(user_id=user_id, obj_key=self.OPERATION_CHAIN_CACHE_KEY)
        most_recent_operation = operation_chain[-1] if operation_chain else None
        return most_recent_operation
    
    def get_dataset_list(self, user_id):
        debug_print()
        return self.get_user_cache_obj(user_id=user_id, obj_key=self.DATASETS_CACHE_KEY)

    def get_operation_chain(self, user_id):
        debug_print()
        return self.get_user_cache_obj(user_id=user_id, obj_key=self.OPERATION_CHAIN_CACHE_KEY)

    def clear_dataset_list(self, user_id):
        debug_print()
        self.cache_user_obj(user_id=user_id, obj_key=self.DATASETS_CACHE_KEY, obj_val=[])

    def clear_operation_chain(self, user_id):
        debug_print()
        self.cache_user_obj(user_id=user_id, obj_key=self.OPERATION_CHAIN_CACHE_KEY, obj_val=[])

    def delete_most_recent_dataset(self, user_id):
        debug_print()
        datasets = self.get_user_cache_obj(user_id=user_id, obj_key=self.DATASETS_CACHE_KEY)
        if datasets:
            datasets.pop()
            self.cache_user_obj(user_id=user_id, obj_key=self.DATASETS_CACHE_KEY, obj_val=datasets)
        else:
            debug_print("No dataset to delete")

    
    def delete_most_recent_operation(self, user_id):
        debug_print()
        operation_chain = self.get_user_cache_obj(user_id=user_id, obj_key=self.OPERATION_CHAIN_CACHE_KEY)
        if operation_chain:
            operation_chain.pop()
            self.cache_user_obj(user_id=user_id, obj_key=self.OPERATION_CHAIN_CACHE_KEY, obj_val=operation_chain)
        else:
            debug_print("No operation to delete")

    def delete_dataset_list(self, user_id):
        debug_print()
        self.delete_user_cache_obj(user_id=user_id, obj_key=self.DATASETS_CACHE_KEY)

    def delete_operation_chain(self, user_id):
        debug_print()
        self.delete_user_cache_obj(user_id=user_id, obj_key=self.OPERATION_CHAIN_CACHE_KEY)
