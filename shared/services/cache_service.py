from django.core.cache import cache
from shared.logger import debug_print, debug_print_vars
from shared.util import log_vars_vals_cls, catch_exceptions_cls

#@log_vars_vals_cls(exclude=None)
@catch_exceptions_cls(exception_return_value={"success": False})
class CacheService:
    MAX_CACHE_LEN = 10  # Max items in a specific cache object for a user

    def __init__(self):
        pass

    def get_cache_key(self, user_id):
        # Returns the cache key for a given user.
        return f"{user_id}_cache"
    
    def get_user_cache(self, user_id):
        cache_key = self.get_cache_key(user_id)
        user_cache = cache.get(cache_key, {})
        return user_cache
    
    def get_user_cache_obj(self, user_id, obj_key):
        user_cache = self.get_user_cache(user_id)
        user_cache_obj = user_cache.get(obj_key, None)
        return user_cache_obj
    
    def create_empty_user_cache(self, user_id, user_cache={}):
        cache_key = self.get_cache_key(user_id)
        cache.set(key=cache_key, value=user_cache, timeout=3600)

    def cache_user(self, user_id, user_cache):
        cache_key = self.get_cache_key(user_id)
        cache.set(key=cache_key, value=user_cache, timeout=3600)

    def cache_user_obj(self, user_id, obj_key, obj_val):
        user_cache = self.get_user_cache(user_id=user_id)
        user_cache[obj_key] = obj_val
        cache_key = self.get_cache_key(user_id)
        cache.set(key=cache_key, value=user_cache, timeout=3600)

    def delete_user_cache(self, user_id):
        cache_key = self.get_cache_key(user_id)
        cache.delete(key=cache_key)
        
    def delete_user_cache_obj(self, user_id, obj_key):
        user_cache = self.get_user_cache(user_id)
        user_cache.pop(obj_key, None)
        self.cache_user(user_id=user_id, user_cache=user_cache)
