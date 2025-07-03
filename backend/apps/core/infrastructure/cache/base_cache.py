# shared/services/cache_service.py

from django.core.cache import cache
from typing import Optional

class BaseCache:
    """
    Simple key→value cache abstraction over Django’s cache backend.
    All values must be JSON‐serializable.
    """
    DEFAULT_TIMEOUT = 3600
    def __init__(self, default_timeout: int = DEFAULT_TIMEOUT):
        self.default_timeout = default_timeout

    def build_cache_key(
            self, 
            user_id: int, 
            service: str, 
            resource: str, 
            attribute: Optional[str]=None
        ) -> str:
        if attribute:
            key = f"{service}:{user_id}:{resource}:{attribute}"
        else:
            key = f"{service}:{user_id}:{resource}:"
        return key

    def get(self, key: str, default=None):
        return cache.get(key, default)

    def set(self, key: str, value, timeout: int = DEFAULT_TIMEOUT):
        """
        Store `value` under `key`, expiring after `timeout` seconds.
        If timeout is None, uses self.default_timeout.
        """
        cache.set(key, value, timeout or self.default_timeout)

    def update(self, key, value):
        cache.set(key, value)

    def clear(self, key: str, cleared_value = None):
        """
        Removes the value at key, replacing a None value
        """
        cache.set(key, cleared_value)

    def delete(self, key: str):
        """
        Deletes the key value pair at key
        """
        cache.delete(key)

    def flush_cache(self):
        """
        Removes everything from the cache
        """
        cache.clear()

