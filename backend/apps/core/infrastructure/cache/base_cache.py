# shared/services/cache_service.py

from django.core.cache import cache
from typing import Optional, Type, Dict, List, Any, cast, Mapping, ClassVar, get_args, get_origin, get_type_hints
from enum import Enum
from core.infrastructure.cache.cache_resource import CacheResource, RES
from core.infrastructure.cache.resource_attribute import ResourceAttribute, ATR

class BaseCache:
    """
    Simple key→value cache abstraction over Django’s cache backend.
    All values must be JSON‐serializable.
    """


    def get_typed(self, key: str, typ: Type[T], default: Optional[T] = None) -> Optional[T]:
        val = cache.get(key, default)
        return cast(Optional[T], val)

    RESOURCE_MAP:    ClassVar[Mapping[Any, Type[Any]]]
    ATTRIBUTE_MAP:   ClassVar[Mapping[Any, Mapping[Any, Type[Any]]]]
    DEFAULT_TIMEOUT = 3600
    def __init__(
        self, 
        service: str, 
        resource_map: Optional[Dict[Any, Type]] = None,
        attribute_map: Optional[Dict[Any, Dict[Any, Type]]] = None,
        default_timeout: int = DEFAULT_TIMEOUT
    ):
        self.default_timeout = default_timeout
        self.service = service
        self.resources: Dict[str, CacheResource] = {}
        
        for raw_name, raw_type in self.RESOURCE_MAP.items():
            name = raw_name.value if isinstance(raw_name, Enum) else raw_name
            res  = self.attach_resource(name, raw_type)
            self.resources[name] = res

    def __init_subclass__(cls, **kwargs):
        super().__init_subclass__(**kwargs)
         # 1) Auto-wire any CacheResource[T] annotations into @property getters
        hints = get_type_hints(cls, include_extras=True)
        for attr_name, hint in hints.items():
            origin = get_origin(hint)
            if origin is CacheResource:
                # e.g. hint == CacheResource[OperationChain]
                data_type = get_args(hint)[0]
                def make_prop(name: str, dt: Type[Any]):
                    def prop(self):
                        # lazily create & cache the resource
                        if name not in self.__dict__:
                            cr = self.attach_resource(name, dt)
                            # wire attributes if any declared
                            for fld, fld_type in self.ATTRIBUTE_MAP.get(name, {}).items():
                                helper = cr.attach_attribute(fld, fld_type)
                                setattr(cr, fld, helper)
                            self.__dict__[name] = cr
                        return self.__dict__[name]
                    return property(prop)
                setattr(cls, attr_name, make_prop(attr_name, data_type))



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
    
    def attach_resource(self, name: str, _type: Type) -> CacheResource:
        if hasattr(self, name):
            return getattr(self, name)
        resource = CacheResource(self, name, _type)
        setattr(self, name, resource)
        self.resources[name] = resource
        return resource
    
    def attach_resource_attribute(self, res_name: str, attr_name: str, attr_type: Type) -> ResourceAttribute:
        resource: CacheResource|None = getattr(self, res_name, None)
        if resource is None:
            raise Exception(f"Unfound Resource: {res_name}")
        resource_attribute = resource.attach_attribute(attr_name, attr_type)
        return resource_attribute
    
    def attach_resources_from_map(self, resource_type_map: Dict[Any, Type]) -> List[CacheResource]:
        resources: List[CacheResource] = []
        for name, _type in resource_type_map.items():
            if isinstance(name, Enum):
                name = name.value
            resource = self.attach_resource(name, _type)
            resources.append(resource)
        return resources
    
    def attach_resource_attributes_from_map(self, resource: str, attribute_type_map: Dict[Any, Type]) -> List[ResourceAttribute]:
        attributes: List[ResourceAttribute] = []
        for name, _type in attribute_type_map.items():
            if isinstance(name, Enum):
                name = name.value
            attribute = self.attach_resource_attribute(resource, name, _type)
            attributes.append(attribute)
        return attributes 

    def get(self, key: str, default=None):
        return cache.get(key, default)

    def set(self, key: str, value, timeout: int = DEFAULT_TIMEOUT):
        cache.set(key, value, timeout or self.default_timeout)

    def update(self, key, value):
        cache.set(key, value)

    def clear(self, key: str, cleared_value = None):
        cache.set(key, cleared_value)

    def delete(self, key: str):
        cache.delete(key)

    def flush_cache(self):
        cache.clear()


    


