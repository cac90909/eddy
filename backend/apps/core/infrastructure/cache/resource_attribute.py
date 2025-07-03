from typing import Any, Optional, Generic, TypeVar, Type
from core.infrastructure.cache.cache_resource import CacheResource

ATR = TypeVar("ATR")

class ResourceAttribute(Generic[ATR]):
    """
    A helper for per-attribute caching under a ResourceCache.
    Binds a fixed attribute name, so callers need only supply user_id.
    """
    def __init__(self, parent: CacheResource, attribute: str, _type: Type[ATR]):
        self._parent = parent
        self._attribute = attribute
        self._type = _type

    def get(self, user_id: int) -> Any:
        return self._parent.get(user_id, self._attribute)

    def set(self, user_id: int, value: Any, ttl: Optional[int] = None) -> None:
        return self._parent.set(user_id, value, self._attribute, ttl)

    def delete(self, user_id: int) -> None:
        return self._parent.delete(user_id, self._attribute)
    
    def update(self, user_id: int, value: Any) -> None:
        return self._parent.update(user_id, value, self._attribute)

    def clear(self, user_id: int) -> None:
        return self._parent.clear(user_id, self._attribute)