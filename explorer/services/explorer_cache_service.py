from shared.logger import debug_print, debug_print_vars
from shared.services.cache_service import CacheService

class ExplorerCacheService(CacheService):

    def __init__(self):
        super().__init__()

    MAX_CACHE_LEN = 10  # Max items in a specific cache object for a user

    #Unused Naming Vars At The Moment, Integrate Later
    DATASETS_CACHE_KEY = "datasets"
    OPERATION_CHAIN_CACHE_KEY = "operation_chain"

    def create_empty_explorer_cache(self, user_id):
        debug_print_vars(user_id=user_id)
        if not self.get_user_cache(user_id=user_id):
            debug_print("User Cache Did Not Exist - Creating New User Cache")
            self.create_empty_user_cache(user_id=user_id)
        self.cache_user_obj(user_id=user_id, obj_key="datasets", obj_val=[])
        self.cache_user_obj(user_id=user_id, obj_key="operation_chain", obj_val=[])

    def cache_dataset_item(self, user_id, dataset):
        debug_print_vars(user_id=user_id, dataset=dataset)
        datasets = self.get_user_cache_obj(user_id=user_id, obj_key="datasets")
        if len(datasets) >= self.MAX_CACHE_LEN:
            datasets.pop(0)
        datasets.append(dataset)
        self.cache_user_obj(user_id=user_id, obj_key="datasets", obj_val=datasets)

    def cache_operation_item(self, user_id, operation):
        debug_print_vars(user_id=user_id, operation=operation)
        operation_chain = self.get_user_cache_obj(user_id=user_id, obj_key="operation_chain")
        if len(operation_chain) >= self.MAX_CACHE_LEN:
            operation_chain.pop(0)
        operation_chain.append(operation)
        self.cache_user_obj(user_id=user_id, obj_key="operation_chain", obj_val=operation_chain)

    def cache_dataset_list(self, user_id, dataset_list):
        debug_print_vars(user_id=user_id, dataset_list=dataset_list)
        if len(dataset_list) > self.MAX_CACHE_LEN:
            dataset_list = dataset_list[-self.MAX_CACHE_LEN:]
        self.cache_user_obj(user_id=user_id, obj_key="datasets", obj_val=dataset_list)

    def cache_operation_chain(self, user_id, operation_chain):
        debug_print_vars(user_id=user_id, operation_chain=operation_chain)
        if len(operation_chain) > self.MAX_CACHE_LEN:
            operation_chain = operation_chain[-self.MAX_CACHE_LEN:]
        self.cache_user_obj(user_id=user_id, obj_key="operation_chain", obj_val=operation_chain)

    def get_most_recent_dataset(self, user_id):
        debug_print_vars(user_id=user_id)
        return self.get_user_cache_obj(user_id=user_id, obj_key="datasets")[-1]
    
    def get_most_recent_operation(self, user_id):
        debug_print_vars(user_id=user_id)
        return self.get_user_cache_obj(user_id=user_id, obj_key="operation_chain")[-1]
    
    def get_dataset_list(self, user_id):
        debug_print_vars(user_id=user_id)
        return self.get_user_cache_obj(user_id=user_id, obj_key="datasets")

    def get_operation_chain(self, user_id):
        debug_print_vars(user_id=user_id)
        return self.get_user_cache_obj(user_id=user_id, obj_key="operation_chain")

    def clear_dataset_list(self, user_id):
        debug_print_vars(user_id=user_id)
        self.cache_user_obj(user_id=user_id, obj_key="datasets", obj_val=[])

    def clear_operation_chain(self, user_id):
        debug_print_vars(user_id=user_id)
        self.cache_user_obj(user_id=user_id, obj_key="operation_chain", obj_val=[])

    def delete_most_recent_dataset(self, user_id):
        debug_print_vars(user_id=user_id)
        datasets = self.get_user_cache_obj(user_id=user_id, obj_key="datasets")
        datasets.pop()
        self.cache_user_obj(user_id=user_id, obj_key="datasets", obj_val=datasets)
    
    def delete_most_recent_operation(self, user_id):
        debug_print_vars(user_id=user_id)
        operation_chain = self.get_user_cache_obj(user_id=user_id, obj_key="operation_chain")
        operation_chain.pop()
        self.cache_user_obj(user_id=user_id, obj_key="operation_chain", obj_val=operation_chain)

    def delete_dataset_list(self, user_id):
        debug_print_vars(user_id=user_id)
        self.delete_user_cache_obj(user_id=user_id, obj_key="datasets")

    def delete_operation_chain(self, user_id):
        debug_print_vars(user_id=user_id)
        self.delete_user_cache_obj(user_id=user_id, obj_key="operation_chain")
    



    