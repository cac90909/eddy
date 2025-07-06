from typing import List, Dict, Any
from functools import cached_property

from core.domain.operation.structures.operation_chain import OperationChain
from backend.apps.core.infrastructure.cache.cache_namespace import CacheNamespace
from core.infrastructure.cache.cache_resource import CacheResource
from explorer.domain.structures.metadata import OperationMetadata

class ExplorerCache(CacheNamespace):

    SERVICE        = "explorer"
    CHAIN_RESOURCE = "chain"
    META_RESOURCE  = "meta"

    def __init__(self):
        super().__init__(self.SERVICE)
        self.chain: CacheResource[OperationChain] = self.attach_resource(self.CHAIN_RESOURCE, OperationChain([]), OperationChain)
        self.meta: CacheResource[OperationMetadata] = self.attach_resource(self.META_RESOURCE, OperationMetadata(), OperationMetadata)
        
        
    
        
    


    

