from django.core.cache import cache
from shared.logger import debug_print


class CacheService:
    MAX_CACHE_DATASETS = 3  # Max datasets for both auto and manual cache

    #Formats user id into a valid key
    def get_cache_key(self, user_id):
        return f"{user_id}_cache"

    #Performs either a manual or auto cache
    def cache_data(self, user_id, data):
        """
        Caches data for a user. Automatically manages the size of the cache.

        Args:
            user_id (int): User's ID.
            data: The dataset to cache (e.g., list of dictionaries or any serializable object).
            cache_type (str): The type of cache ('manual' or 'auto').
        """
        user_cache_key = self.get_cache_key(user_id)
        user_cache = cache.get(user_cache_key, [])

        if len(user_cache) >= self.MAX_CACHE_DATASETS:
            user_cache.pop(0)  # Remove the oldest cached dataset

        user_cache.append(data)
        cache.set(user_cache_key, user_cache, timeout=3600)  # Cache for 1 hour
        debug_print(f"{user_cache_key}: ({len(user_cache)}/{self.MAX_CACHE_DATASETS}) cached {data.count()}")

    #Retrieving the most recently cached dataset (at the top of the cache stack)
    def get_most_recent_cache_data(self, user_id):
        user_cache_key = self.get_cache_key(user_id, )
        cached_data = cache.get(user_cache_key, [])

        if not cached_data:
            raise ValueError(f"No cached data found for key: {user_cache_key}")

        debug_print("Retrieved Most Recent Data From Cache", {user_cache_key: cached_data[-1]})
        return cached_data[-1]

    #Clears both auto and manual cache if a cache type is not passed
    def clear_cache(self, user_id):
        user_cache_key = self.get_cache_key(user_id)
        cache.delete(user_cache_key)
        debug_print("Cleared All Cache for User: ", user_cache_key)
