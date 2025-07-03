from typing import List, Dict, Any
from functools import cached_property

from core.domain.operation.structures.operation_chain import OperationChain
from core.infrastructure.cache.base_cache import BaseCache
from core.infrastructure.cache.cache_resource import CacheResource
from core.infrastructure.cache.resource_attribute import ResourceAttribute

class ExplorerCache(BaseCache):

    SERVICE        = "explorer"
    CHAIN_RESOURCE = "operation_chain"
    META_RESOURCE  = "metadata"
    

    def __init__(self):
        super().__init__()

    @property
    def _chain_key(self):
        return self._chain_key(self.CHAIN_RESOURCE)
    
    @property
    def _meta_key(self):
        return self._chain_key(self.META_RESOURCE)
    
    @cached_property
    def chain(self):
        return CacheResource(self, self.SERVICE, self.CHAIN_RESOURCE, OperationChain)
    
    @cached_property
    def meta(self):
        return CacheResource(self, self.SERVICE, self.META_RESOURCE, Dict[str,Any])
    


    

