from typing import Any, Optional, TypeVar, Generic, Type, Dict, List
from core.infrastructure.cache.base_cache import BaseCache
from core.infrastructure.cache.resource_attribute import ResourceAttribute, ATR

RES = TypeVar("RES") #Resource Level Generic

class CacheResource(Generic[RES]):

    def __init__(self, parent_cache: 'BaseCache', name: str, _type: Type[RES]) -> None:
        self.parent_cache = parent_cache
        self.name = name
        self._type = _type
        self.attributes: Dict[str, ResourceAttribute] = {}

    def _key(self, user_id: int, attributename: Optional[str] = None) -> str:
        return self.parent_cache.build_cache_key(user_id, self.parent_cache.service, self.name, attributename)

    def get(self, user_id: int, attributename: Optional[str] = None) -> RES:
        key = self._key(user_id, attributename)
        return self.parent_cache.get(key)

    def set(self, user_id: int, value: Any, attributename: Optional[str] = None, ttl: Optional[int] = None) -> None:
        key = self._key(user_id, attributename)
        if ttl is None:
            ttl = self.parent_cache.default_timeout
        self.parent_cache.set(key, value, ttl)

    def delete(self, user_id: int, attributename: Optional[str] = None) -> None:
        key = self._key(user_id, attributename)
        self.parent_cache.delete(key)

    def clear(self, user_id: int, attributename: str, cleared_value: Optional[Any]=None) -> None:
        key = self._key(user_id, attributename)
        self.parent_cache.clear(key, cleared_value)

    def update(self, user_id: int, value: Any, attributename: Optional[str] = None) -> None:
        key = self._key(user_id, attributename)
        self.parent_cache.set(key, value)

    def attribute(self, name: str, _type: Type[ATR]) -> ResourceAttribute[ATR]:
        if hasattr(self, name):
            return getattr(self, name)
        resource_attribute = ResourceAttribute(self, name, _type)
        setattr(self, name, resource_attribute)
        self.attributes[name] = resource_attribute
        return resource_attribute
    
    def get_all(self, user_id: int) -> List[ResourceAttribute]:
        return [res_attr.get(user_id) for _, res_attr in self.attributes.items()]
    
    def delete_all(self, user_id: int):
        [res_attr.delete(user_id) for _, res_attr in self.attributes.items()]
    
    def clear_all(self, user_id: int):
        [res_attr.get(user_id) for _, res_attr in self.attributes.items()]
    
    def set_all(self, user_id: int):
        [res_attr.get(user_id) for _, res_attr in self.attributes.items()]

    
    @staticmethod
    def assign_attribute_as_property(attribute_name: str, attribute_type: Type[ATR]) -> property:
        def getter(self: CacheResource[ATR]) -> ResourceAttribute[ATR]:
            return self.attribute(attribute_name, attribute_type)
        return property(getter)











