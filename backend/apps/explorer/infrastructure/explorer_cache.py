from typing import List, Dict, Any
from functools import cached_property

from core.domain.operation.structures.operation_chain import OperationChain
from core.infrastructure.cache.base_cache import BaseCache
from core.infrastructure.cache.cache_resource import CacheResource
from core.infrastructure.cache.resource_attribute import ResourceAttribute
from explorer.domain.structures.shape_dict import ShapeDict

from explorer.infrastructure.meta_resource import MetaResource
from explorer.domain.enums.meta_fields import MetaFields
from explorer.domain.maps.meta_field_types import META_FIELD_TYPES

class ExplorerCache(BaseCache):

    SERVICE        = "explorer"
    CHAIN_RESOURCE = "chain"
    META_RESOURCE  = "meta"


    def __init__(self):
        super().__init__(service=self.SERVICE)

    
    def chain_key(self, user_id):
        return self.build_cache_key(user_id, self.SERVICE, self.CHAIN_RESOURCE)
    def meta_last_updated_key(self, user_id):
        return self.build_cache_key(user_id, self.SERVICE, self.META_RESOURCE, MetaFields.last_updated.value)
    def meta_operation_count_key(self, user_id):
        return self.build_cache_key(user_id, self.SERVICE, self.META_RESOURCE, MetaFields.operation_count.value)
    def meta_full_shape_key(self, user_id):
        return self.build_cache_key(user_id, self.SERVICE, self.META_RESOURCE, MetaFields.full_shape.value)
    def meta_current_shape_key(self, user_id):
        return self.build_cache_key(user_id, self.SERVICE, self.META_RESOURCE, MetaFields.current_shape.value)

    




    

        
    
        
    


    

