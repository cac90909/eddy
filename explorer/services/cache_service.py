from django.core.cache import cache
from shared.logger import debug_print


class CacheService:
    MAX_CACHE_DATASETS = 3  # Max datasets for both auto and manual cache

    #Formats the cache type and user id into a valid key
    def _get_cache_key(self, user_id, cache_type):
        """
        Generates the cache key based on the user ID and cache type.
        """
        if cache_type not in ["manual", "auto"]:
            debug_print("Invalid Cache Type", cache_type)
            raise ValueError(f"Invalid cache type: {cache_type}")
        return f"{user_id}_{cache_type}"

    #Performs either a manual or auto cache
    def cache_data(self, user_id, data, cache_type="auto"):
        """
        Caches data for a user. Automatically manages the size of the cache.

        Args:
            user_id (int): User's ID.
            data: The dataset to cache (e.g., list of dictionaries or any serializable object).
            cache_type (str): The type of cache ('manual' or 'auto').
        """
        user_cache_key = self._get_cache_key(user_id, cache_type)
        user_cache = cache.get(user_cache_key, [])

        if len(user_cache) >= self.MAX_CACHE_DATASETS:
            user_cache.pop(0)  # Remove the oldest cached dataset

        user_cache.append(data)
        cache.set(user_cache_key, user_cache, timeout=3600)  # Cache for 1 hour
        debug_print(f"{cache_type} ({len(user_cache)}/{self.MAX_CACHE_DATASETS}) cached {data.count()} rows on key {user_cache_key}")

    #Retrieving a session saved manual cache dataset
    def get_cache_data(self, user_id, cache_num, cache_type="manual" ):
        """
        Retrieves a specific cached dataset by its index.

        Args:
            user_id (int): User's ID.
            cache_type (str): The type of cache ('manual' or 'auto').
            cache_num (int): The index of the dataset in the cache.

        Returns:
            The requested cached dataset.
        """
        user_cache_key = self._get_cache_key(user_id, cache_type)
        cached_data = cache.get(user_cache_key, [])

        if cache_num >= len(cached_data) or cache_num < 0:
            raise IndexError(f"Cache index {cache_num} out of range for {user_cache_key}")

        debug_print("Retrieved Specific Data From Cache", {user_cache_key: cached_data[cache_num]})
        return cached_data[cache_num]

    #Retrieving the most recent auto cache dataset (at the top of the cache stack)
    def get_most_recent_cache_data(self, user_id, cache_type="auto"):
        """
        Retrieves the most recent dataset from the cache.

        Args:
            user_id (int): User's ID.
            cache_type (str): The type of cache ('manual' or 'auto').

        Returns:
            The most recent cached dataset.
        """
        user_cache_key = self._get_cache_key(user_id, cache_type)
        cached_data = cache.get(user_cache_key, [])

        if not cached_data:
            raise ValueError(f"No cached data found for key: {user_cache_key}")

        debug_print("Retrieved Most Recent Data From Cache", {user_cache_key: cached_data[-1]})
        return cached_data[-1]

    #Clears both auto and manual cache if a cache type is not passed
    def clear_cache(self, user_id, cache_type=None):
        """
        Clears all or specific type of cache for a user.

        Args:
            user_id (int): User's ID.
            cache_type (str, optional): The type of cache to clear ('manual' or 'auto').
                                        If None, clears all types of cache for the user.
        """
        if cache_type:
            user_cache_key = self._get_cache_key(user_id, cache_type)
            cache.delete(user_cache_key)
            debug_print("Cleared Specific Cache", user_cache_key)
        else:
            for type_key in ["manual", "auto"]:
                user_cache_key = self._get_cache_key(user_id, type_key)
                cache.delete(user_cache_key)
                debug_print("Cleared All Cache for User", user_cache_key)
