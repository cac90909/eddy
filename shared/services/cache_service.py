from django.core.cache import cache
from shared.logger import debug_print


#Getters, Setters, and Deleters
#Operating at 3 Scopes: User, User Object, User Object Item
#Ex: user: 31, object: operation_chain, item: 3
#{31: {datasets: [...,...,...], operation_chain: [...,...,...], snapshots: [...,...,...]}}
from shared.logger import debug_print
from django.core.cache import cache

class CacheService:
    MAX_CACHE_LEN = 10  # Max items in a specific cache object for a user

    # Formats user id into a valid key
    def get_cache_key(self, user_id):
        debug_print("get_cache_key called with:", {"user_id": user_id})
        return f"{user_id}_cache"
    
    def get_user_cache(self, user_id):
        debug_print("get_user_cache called with:", {"user_id": user_id})
        cache_key = self.get_cache_key(user_id)
        return cache.get(cache_key, [])
    
    def get_user_cache_obj(self, user_id, obj_name):
        debug_print("get_user_cache_obj called with:", {"user_id": user_id, "obj_name": obj_name})
        user_cache = self.get_user_cache(user_id)
        return user_cache.get(obj_name, [])
    
    def get_user_cache_obj_item(self, user_id, obj_name, item_index):
        debug_print("get_user_cache_obj_item called with:", {"user_id": user_id, "obj_name": obj_name, "item_index": item_index})
        user_cache_obj = self.get_user_cache_obj(user_id, obj_name)
        return user_cache_obj[item_index]
    
    def get_most_recent_user_cache_obj_item(self, user_id, obj_name):
        debug_print("get_most_recent_user_cache_obj_item called with:", {"user_id": user_id, "obj_name": obj_name})
        user_cache_obj = self.get_user_cache_obj(user_id, obj_name)
        return user_cache_obj[-1]
    

    def cache_user(self, user_id, user_data):
        debug_print("cache_user called with:", {"user_id": user_id, "user_data": user_data})
        cache_key = self.get_cache_key(user_id)
        cache.set(key=cache_key, value=user_data, timeout=3600)

    def cache_user_obj(self, user_id, obj_key, obj_val):
        debug_print("cache_user_obj called with:", {"user_id": user_id, "obj_key": obj_key, "obj_val": obj_val})
        user_cache = self.get_user_cache(user_id)
        user_cache[obj_key] = obj_val
        cache_key = self.get_cache_key(user_id)
        cache.set(key=cache_key, value=user_cache, timeout=3600)

    def cache_user_obj_item(self, user_id, obj_name, item_val):
        debug_print("cache_user_obj_item called with:", {"user_id": user_id, "obj_name": obj_name, "item_val": item_val})
        user_cache_obj = self.get_user_cache_obj(user_id=user_id, obj_name=obj_name)
        if len(user_cache_obj) >= self.MAX_CACHE_LEN:
            user_cache_obj.pop(0)
        user_cache_obj.append(item_val)
        self.cache_user_obj(user_id=user_id, obj_key=obj_name, obj_val=user_cache_obj)
   
    def delete_user_cache(self, user_id):
        debug_print("clear_user_cache called with:", {"user_id": user_id})
        cache_key = self.get_cache_key(user_id=user_id)
        cache.delete(key=cache_key)
        
    def delete_user_cache_obj(self, user_id, obj_name):
        debug_print("clear_user_cache_obj called with:", {"user_id": user_id, "obj_name": obj_name})
        user_cache = self.get_user_cache(user_id=user_id)
        user_cache.pop(obj_name, None)
        self.cache_user(user_id=user_id, user_data=user_cache)
  
    def delete_user_cache_obj_item(self, user_id, obj_name, item_index):
        debug_print("clear_user_cache_obj_item called with:", {"user_id": user_id, "obj_name": obj_name, "item_index": item_index})
        user_cache_obj = self.get_user_cache_obj(user_id=user_id, obj_name=obj_name)
        user_cache_obj.pop(item_index)
        self.cache_user_obj(user_id=user_id, obj_key=obj_name, obj_val=user_cache_obj)
