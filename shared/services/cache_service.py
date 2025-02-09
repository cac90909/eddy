from django.core.cache import cache
from shared.logger import debug_print


#Getters, Setters, and Deleters
#Operating at 3 Scopes: User, User Object, User Object Item
#Ex: user: 31, object: operation_chain, item: 3
#{31: {datasets: [...,...,...], operation_chain: [...,...,...], snapshots: [...,...,...]}}
from shared.logger import debug_print, debug_print_vars
from django.core.cache import cache

class CacheService:

    def __init__(self):
        pass

    def get_cache_key(self, user_id):
        debug_print_vars(user_id=user_id)
        return f"{user_id}_cache"
    
    def get_user_cache(self, user_id):
        debug_print_vars(user_id=user_id)
        cache_key = self.get_cache_key(user_id)
        return cache.get(cache_key, [])
    
    def get_user_cache_obj(self, user_id, obj_key):
        debug_print_vars(user_id=user_id, obj_key=obj_key)
        user_cache = self.get_user_cache(user_id=user_id)
        return user_cache.get(obj_key, [])

    def create_empty_user_cache(self, user_id):
        debug_print_vars(user_id=user_id)
        cache_key = self.get_cache_key(user_id)
        cache.set(key=cache_key, value={}, timeout=3600)

    def cache_user(self, user_id, user_cache):
        debug_print_vars(user_id=user_id, user_cache=user_cache)
        cache_key = self.get_cache_key(user_id)
        cache.set(key=cache_key, value=user_cache, timeout=3600)

    def cache_user_obj(self, user_id, obj_key, obj_val):
        debug_print_vars(user_id=user_id, obj_key=obj_key, obj_val=obj_val)
        user_cache = self.get_user_cache(user_id)
        user_cache[obj_key] = obj_val
        cache_key = self.get_cache_key(user_id)
        cache.set(key=cache_key, value=user_cache, timeout=3600)
   
    def delete_user_cache(self, user_id):
        debug_print_vars(user_id=user_id)
        cache_key = self.get_cache_key(user_id=user_id)
        cache.delete(key=cache_key)
        
    def delete_user_cache_obj(self, user_id, obj_key):
        debug_print_vars(user_id=user_id, obj_key=obj_key)
        user_cache = self.get_user_cache(user_id=user_id)
        user_cache.pop(obj_key, None)
        self.cache_user(user_id=user_id, user_cache=user_cache)
