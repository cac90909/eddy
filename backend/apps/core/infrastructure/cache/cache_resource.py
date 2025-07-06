from django.core.cache import cache
from typing import TypeVar, Generic, Type, TypedDict, Any, List
from dataclasses import dataclass
from core.infrastructure.cache.cache_namespace import CacheNamespace

T = TypeVar("T")

class CacheResource(Generic[T]):

    def __init__(self, parent: CacheNamespace, name, val: T, **kwargs):
        self.name = name
        self.val = val
        self.parent = parent

    def _key(self, user_id: int):
        return self.parent._build_key(user_id, self.name)
        
    def get(self, user_id: int) -> T:
        return cache.get(self._key(user_id), None)
    
    def set(self, user_id: int, value: T):
        cache.set(self._key(user_id), value)

    def clear(self, user_id: int):
        cache.set(self._key(user_id), None)

    def delete(self, user_id: int):
        cache.delete(self._key(user_id))

    def flush_cache(self):
        cache.clear()
