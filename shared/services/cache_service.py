from django.core.cache import cache
from shared.logger import debug_print


#Getters, Setters, and Deleters
#Operating at 3 Scopes: User, User Object, User Object Item
#Ex: user: 31, object: operation_chain, item: 3
#{31: {datasets: [...,...,...], operation_chain: [...,...,...], snapshots: [...,...,...]}}
from shared.logger import debug_print, debug_print_vars
from django.core.cache import cache

class CacheService:
    MAX_CACHE_LEN = 10  # Max items in a specific cache object for a user

    # Formats user id into a valid key
    def get_cache_key(self, user_id):
        debug_print_vars(user_id=user_id)
        return f"{user_id}_cache"
    
    def get_user_cache(self, user_id):
        debug_print_vars(user_id=user_id)
        cache_key = self.get_cache_key(user_id)
        return cache.get(cache_key, [])
    
    def get_user_cache_obj(self, user_id, obj_key):
        debug_print_vars(user_id=user_id, obj_key=obj_key)
        user_cache = self.get_user_cache(user_id)
        return user_cache.get(obj_key, [])
    
    def get_user_cache_obj_item(self, user_id, obj_key, item_index):
        debug_print_vars(user_id=user_id, obj_key=obj_key, item_index=item_index)
        user_cache_obj = self.get_user_cache_obj(user_id, obj_key)
        return user_cache_obj[item_index]
    
    def get_most_recent_user_cache_obj_item(self, user_id, obj_key):
        debug_print_vars(user_id=user_id, obj_key=obj_key)
        user_cache_obj = self.get_user_cache_obj(user_id, obj_key)
        return user_cache_obj[-1]

    def cache_user(self, user_id, user_obj):
        debug_print_vars(user_id=user_id, user_obj=user_obj)
        cache_key = self.get_cache_key(user_id)
        cache.set(key=cache_key, value=user_obj, timeout=3600)

    def cache_user_obj(self, user_id, obj_key, obj_val):
        debug_print_vars(user_id=user_id, obj_key=obj_key, obj_val=obj_val)
        user_cache = self.get_user_cache(user_id)
        user_cache[obj_key] = obj_val
        cache_key = self.get_cache_key(user_id)
        cache.set(key=cache_key, value=user_cache, timeout=3600)

    def cache_user_obj_item(self, user_id, obj_key, item_val, item_index):
        debug_print_vars(user_id=user_id, obj_key=obj_key, item_val=item_val)
        user_cache_obj = self.get_user_cache_obj(user_id=user_id, obj_key=obj_key)
        if len(user_cache_obj) >= self.MAX_CACHE_LEN:
            user_cache_obj.pop(0)
        user_cache_obj.insert(item_val, item_index)
        self.cache_user_obj(user_id=user_id, obj_key=obj_key, obj_val=user_cache_obj)
   
    def delete_user_cache(self, user_id):
        debug_print_vars(user_id=user_id)
        cache_key = self.get_cache_key(user_id=user_id)
        cache.delete(key=cache_key)
        
    def delete_user_cache_obj(self, user_id, obj_key):
        debug_print_vars(user_id=user_id, obj_key=obj_key)
        user_cache = self.get_user_cache(user_id=user_id)
        user_cache.pop(obj_key, None)
        self.cache_user(user_id=user_id, user_data=user_cache)
  
    def delete_user_cache_obj_item(self, user_id, obj_key, item_index):
        debug_print_vars(user_id=user_id, obj_key=obj_key, item_index=item_index)
        user_cache_obj = self.get_user_cache_obj(user_id=user_id, obj_key=obj_key)
        user_cache_obj.pop(item_index)
        self.cache_user_obj(user_id=user_id, obj_key=obj_key, obj_val=user_cache_obj)