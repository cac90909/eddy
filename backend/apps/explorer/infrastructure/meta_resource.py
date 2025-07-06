from core.infrastructure.cache.cache_resource import CacheResource
from core.infrastructure.cache.resource_attribute import ResourceAttribute
from enum import Enum
from typing import Dict, Type, TypedDict
from datetime import datetime
from explorer.domain.enums.meta_fields import MetaFields
from explorer.domain.maps.meta_field_types import META_FIELD_TYPES
from explorer.domain.structures.shape_dict import ShapeDict

class MetaResource(CacheResource):

    @property
    def last_updated(self):
        return self.attribute(
            MetaFields.last_updated.value, 
            META_FIELD_TYPES[MetaFields.last_updated]
    )

    @property
    def operation_count(self) -> ResourceAttribute[int]:
        return self.attribute(
            MetaFields.operation_count.value, 
            META_FIELD_TYPES[MetaFields.operation_count]
    )

    @property
    def full_shape(self):
        return self.attribute(
            MetaFields.full_shape.value, 
            META_FIELD_TYPES[MetaFields.full_shape]
    )

    @property
    def current_shape(self):
        return self.attribute(
            MetaFields.current_shape.value, 
            META_FIELD_TYPES[MetaFields.current_shape]
    )
