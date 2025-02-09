import functools
from shared.logger import debug_print_vars  # (Assuming this is now used only internally by decorators)
from shared.services.cache_service import CacheService

# Assume these class-level decorators have been defined in your shared.utilities module:
# They apply logging (log_values) and exception catching (catch_exceptions_class) to all methods.
from shared.util import log_vars_vals_cls, catch_exceptions_cls

@log_vars_vals_cls()  # By default, this applies to all callable methods (excluding special methods)
@catch_exceptions_cls(exception_return_value={"success": False})
class ExplorerCacheService(CacheService):
    MAX_CACHE_LEN = 10  # Max items in a specific cache object for a user

    # Named variables for subcache keys
    DATASETS_CACHE_KEY = "datasets"
    OPERATION_CHAIN_CACHE_KEY = "operation_chain"

    def create_empty_explorer_cache(self, user_id):
        # If no user cache exists, create an empty one
        if not self.get_user_cache(user_id=user_id):
            # Here, the base CacheService method to create an empty cache is called.
            self.create_empty_user_cache(user_id=user_id)
        # Create explorer-specific sub-objects
        self.cache_user_obj(user_id=user_id, obj_key=self.DATASETS_CACHE_KEY, obj_val=[])
        self.cache_user_obj(user_id=user_id, obj_key=self.OPERATION_CHAIN_CACHE_KEY, obj_val=[])
        return {"success": True}

    def cache_dataset_item(self, user_id, dataset):
        datasets = self.get_user_cache_obj(user_id=user_id, obj_key=self.DATASETS_CACHE_KEY)
        if len(datasets) >= self.MAX_CACHE_LEN:
            datasets.pop(0)
        datasets.append(dataset)
        self.cache_user_obj(user_id=user_id, obj_key=self.DATASETS_CACHE_KEY, obj_val=datasets)
        return {"success": True}

    def cache_operation_item(self, user_id, operation):
        operation_chain = self.get_user_cache_obj(user_id=user_id, obj_key=self.OPERATION_CHAIN_CACHE_KEY)
        if len(operation_chain) >= self.MAX_CACHE_LEN:
            operation_chain.pop(0)
        operation_chain.append(operation)
        self.cache_user_obj(user_id=user_id, obj_key=self.OPERATION_CHAIN_CACHE_KEY, obj_val=operation_chain)
        return {"success": True}

    def cache_dataset_list(self, user_id, dataset_list):
        if len(dataset_list) > self.MAX_CACHE_LEN:
            dataset_list = dataset_list[-self.MAX_CACHE_LEN:]
        self.cache_user_obj(user_id=user_id, obj_key=self.DATASETS_CACHE_KEY, obj_val=dataset_list)
        return {"success": True}

    def cache_operation_chain(self, user_id, operation_chain):
        if len(operation_chain) > self.MAX_CACHE_LEN:
            operation_chain = operation_chain[-self.MAX_CACHE_LEN:]
        self.cache_user_obj(user_id=user_id, obj_key=self.OPERATION_CHAIN_CACHE_KEY, obj_val=operation_chain)
        return {"success": True}

    def get_most_recent_dataset(self, user_id):
        datasets = self.get_user_cache_obj(user_id=user_id, obj_key=self.DATASETS_CACHE_KEY)
        return datasets[-1] if datasets else None
    
    def get_most_recent_operation(self, user_id):
        operation_chain = self.get_user_cache_obj(user_id=user_id, obj_key=self.OPERATION_CHAIN_CACHE_KEY)
        return operation_chain[-1] if operation_chain else None
    
    def get_dataset_list(self, user_id):
        return self.get_user_cache_obj(user_id=user_id, obj_key=self.DATASETS_CACHE_KEY)

    def get_operation_chain(self, user_id):
        return self.get_user_cache_obj(user_id=user_id, obj_key=self.OPERATION_CHAIN_CACHE_KEY)

    def clear_dataset_list(self, user_id):
        self.cache_user_obj(user_id=user_id, obj_key=self.DATASETS_CACHE_KEY, obj_val=[])
        return {"success": True}

    def clear_operation_chain(self, user_id):
        self.cache_user_obj(user_id=user_id, obj_key=self.OPERATION_CHAIN_CACHE_KEY, obj_val=[])
        return {"success": True}

    def delete_most_recent_dataset(self, user_id):
        datasets = self.get_user_cache_obj(user_id=user_id, obj_key=self.DATASETS_CACHE_KEY)
        if datasets:
            datasets.pop()
            self.cache_user_obj(user_id=user_id, obj_key=self.DATASETS_CACHE_KEY, obj_val=datasets)
            return {"success": True}
        return {"success": False, "error": "No dataset to delete"}
    
    def delete_most_recent_operation(self, user_id):
        operation_chain = self.get_user_cache_obj(user_id=user_id, obj_key=self.OPERATION_CHAIN_CACHE_KEY)
        if operation_chain:
            operation_chain.pop()
            self.cache_user_obj(user_id=user_id, obj_key=self.OPERATION_CHAIN_CACHE_KEY, obj_val=operation_chain)
            return {"success": True}
        return {"success": False, "error": "No operation to delete"}

    def delete_dataset_list(self, user_id):
        self.delete_user_cache_obj(user_id=user_id, obj_key=self.DATASETS_CACHE_KEY)
        return {"success": True}

    def delete_operation_chain(self, user_id):
        self.delete_user_cache_obj(user_id=user_id, obj_key=self.OPERATION_CHAIN_CACHE_KEY)
        return {"success": True}
