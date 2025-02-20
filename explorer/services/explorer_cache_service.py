from django.core.cache import cache
from shared.logger import debug_print_vars, debug_print
from shared.util import log_vars_vals_cls, catch_exceptions_cls


@catch_exceptions_cls(exception_return_value={"success": False})
class ExplorerCacheService():
    MAX_CACHE_LEN = 50  # Max items in a specific cache object for a user

    # Named variables for subcache keys
    UNIVERSAL_DATA_CACHE_KEY = "universal_data"
    OPERATION_CHAIN_CACHE_KEY = "operation_chain"
    
    def get_cache_key(self, user_id):
        # Returns the cache key for a given user.
        return f"{user_id}_cache"
    
    def get_user_cache(self, user_id):
        cache_key = self.get_cache_key(user_id)
        user_cache = cache.get(cache_key, {})
        return user_cache

    def create_empty_explorer_cache(self, user_id):
        #Checking if User has Cache, and Creates One If Doesnt Already Exist
        if not self.get_user_cache(user_id=user_id):
            self.create_empty_user_cache(user_id=user_id)
        user_cache = self.get_user_cache(user_id=user_id)
        user_cache[self.UNIVERSAL_DATA_CACHE_KEY] = []
        user_cache[self.OPERATION_CHAIN_CACHE_KEY] = []
        cache.set(key=self.get_cache_key, value=user_cache, timeout=3600)

    def append_universal_data_and_operation_objs(self, user_id, universal_data_obj, operation_obj):
        user_cache = self.get_user_cache(user_id=user_id)
        universal_data_list, operation_chain_list = user_cache[self.UNIVERSAL_DATA_CACHE_KEY], user_cache[self.OPERATION_CHAIN_CACHE_KEY]
        if len(universal_data_list) >= self.MAX_CACHE_LEN or len(operation_chain_list) >= self.MAX_CACHE_LEN:
            return Exception (f"Exceeded Cache Limit: {self.MAX_CACHE_LEN}")
        else:
            universal_data_list.append(universal_data_obj)
            operation_chain_list.append(operation_obj)

    def delete_most_recent_universal_data_and_operation_obj(self, user_id):
        user_cache = self.get_user_cache(user_id=user_id)
        universal_data_list, operation_chain_list = user_cache[self.UNIVERSAL_DATA_CACHE_KEY], user_cache[self.OPERATION_CHAIN_CACHE_KEY]
        universal_data_list.pop(-1)
        operation_chain_list.pop(-1)

    def empty_explorer_cache(self, user_id):
        self.create_empty_explorer_cache(self, user_id=user_id)

    def get_universal_data_list(self, user_id):
        user_cache = self.get_user_cache(user_id=user_id)
        universal_data_list = user_cache[self.UNIVERSAL_DATA_CACHE_KEY]
        return universal_data_list

    def get_operation_chain_list(self, user_id):
        user_cache = self.get_user_cache(user_id=user_id)
        operation_chain_list = user_cache[self.OPERATION_CHAIN_CACHE_KEY]
        return operation_chain_list
    
    #NOTE: currently cannot operate on non raw universal data, so returning only raw objects
    def get_most_recent_universal_raw_data_obj(self, user_id):
        user_cache = self.get_user_cache(user_id=user_id)
        universal_data_list = user_cache[self.UNIVERSAL_DATA_CACHE_KEY]
        for universal_data_obj in universal_data_list[-1:-1:-1]:
            if universal_data_obj["universal_raw"]:
                return universal_data_obj
        return None


    def get_most_recent_operation_obj(self, user_id):
        user_cache = self.get_user_cache(user_id=user_id)
        operatin_chain_list = user_cache[self.OPERATION_CHAIN_CACHE_KEY]
        return operatin_chain_list[-1] if operatin_chain_list else None

    def delete_explorer_cache_objs(self, user_id):
        user_cache = self.get_user_cache(user_id=user_id)
        user_cache.pop(self.UNIVERSAL_DATA_CACHE_KEY)
        user_cache.pop(self.OPERATION_CHAIN_CACHE_KEY)
