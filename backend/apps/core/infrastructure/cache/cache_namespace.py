from typing import Type, List
from core.infrastructure.cache.cache_resource import CacheResource, T



class CacheNamespace():

    DEFAULT_TIMEOUT = 3600

    def __init__(self, serv:str, **kwargs):
        self.service = serv
        self.resources: List['CacheResource']

    def _build_key(self, user_id: int, resource_name: str) -> str:
        return f'{self.service}:{resource_name}:{user_id}'

    def attach_resource(self, name: str, val: T, _type: Type[T]) -> 'CacheResource[T]':
        cache_res = CacheResource[T](self, name, val)
        self.__setattr__(name, val)
        self.__annotations__[name] = _type
        self.resources.append(cache_res)
        return cache_res


   

