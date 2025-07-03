from typing import Any, Optional, TypeVar, Generic, Type
from core.infrastructure.cache.base_cache import BaseCache
from core.infrastructure.cache.resource_attribute import ResourceAttribute, ATR

RES = TypeVar("RES") #Resource Level Generic

class CacheResource(Generic[RES]):

    def __init__(self, parent: BaseCache, service: str, resource: str, _type: Type[RES]) -> None:
        self._parent = parent
        self._service = service
        self._resource = resource
        self._type = _type

    def _key(self, user_id: int, attribute: Optional[str] = None) -> str:
        return self._parent.build_cache_key(user_id, self._service, self._resource, attribute)

    def get(self, user_id: int, attribute: Optional[str] = None) -> Any:
        key = self._key(user_id, attribute)
        return self._parent.get(key)

    def set(self, user_id: int, value: Any, attribute: Optional[str] = None, ttl: Optional[int] = None) -> None:
        key = self._key(user_id, attribute)
        if ttl is None:
            ttl = self._parent.default_timeout
        self._parent.set(key, value, ttl)

    def delete(self, user_id: int, attribute: Optional[str] = None) -> None:
        key = self._key(user_id, attribute)
        self._parent.delete(key)

    def clear(self, user_id: int, attribute: str, cleared_value: Optional[Any]=None) -> None:
        key = self._key(user_id, attribute)
        self._parent.clear(key, cleared_value)

    def update(self, user_id: int, value: Any, attribute: Optional[str] = None) -> None:
        key = self._key(user_id, attribute)
        self._parent.set(key, value)

    def attribute(self, name: str, _type: Type[ATR]) -> ResourceAttribute:
        if hasattr(self, name):
            return getattr(self, name)
        helper = ResourceAttribute(self, name, _type)
        setattr(self, name, helper)
        return helper
    


