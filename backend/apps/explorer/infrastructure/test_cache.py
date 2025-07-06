from django.core.cache import cache
from typing import TypeVar, Generic, Type, TypedDict, Any, List
from dataclasses import dataclass
from core.domain.operation.structures.operation_chain import OperationChain

T = TypeVar("T")

class BaseCache():

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

    

class CacheResource(Generic[T]):

    def __init__(self, parent: BaseCache, name, val: T, **kwargs):
        self.name = name
        self.val = val
        self.parent = parent

    def _key(self, user_id: int):
        return self.parent._build_key(user_id, self.name)
        
    def get(self, user_id) -> T:
        return cache.get(self._key(user_id))
    
    def set(self, user_id: int, value: T):
        cache.set(self._key(user_id), value)

    def clear(self, user_id: int):
        cache.set(self._key(user_id), None)

    def delete(self, user_id: int):
        cache.delete(self._key(user_id))

    def flush_cache(self):
        cache.clear()

   

@dataclass
class Shape():
    num_rows: int
    num_cols: int

@dataclass
class MetaResponse():
    last_op: int
    num_ops: int
    shape: Shape

class expCache(bCache):

    def __init__(self):
        super().__init__("exp")
        self.chain = self.bind_cacheObj("chain", OperationChain([]), OperationChain)
        self.meta: cacheObj[MetaResponse] = self.bind_cacheObj("meta", MetaResponse, MetaResponse)
        


class cacheUseCase:

    def __init__(self):
        self.cache_ins = expCache()

    def useCase(self):
        meta = self.cache_ins.meta.get()
        meta.shape = Shape(1,1)


