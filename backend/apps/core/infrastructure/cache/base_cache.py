# shared/services/cache_service.py

from django.core.cache import cache

class BaseCache:
    """
    Simple key→value cache abstraction over Django’s cache backend.
    All values must be JSON‐serializable.
    """
    DEFAULT_TIMEOUT = 3600
    def __init__(self, default_timeout: int = DEFAULT_TIMEOUT):
        self.default_timeout = default_timeout

    def get(self, key: str, default=None):
        return cache.get(key, default)

    def set(self, key: str, value, timeout: int = DEFAULT_TIMEOUT):
        """
        Store `value` under `key`, expiring after `timeout` seconds.
        If timeout is None, uses self.default_timeout.
        """
        cache.set(key, value, timeout or self.default_timeout)

    def delete(self, key: str):
        cache.delete(key)
