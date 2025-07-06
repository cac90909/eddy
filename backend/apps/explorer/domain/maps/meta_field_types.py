from typing import Dict, Type
from datetime import datetime
from explorer.domain.enums.meta_fields import MetaFields
from explorer.domain.structures.shape_dict import ShapeDict


META_FIELD_TYPES: Dict[MetaFields, type] = {
    MetaFields.last_updated: datetime,
    MetaFields.operation_count: Type[int],
    MetaFields.full_shape: ShapeDict,
    MetaFields.current_shape: ShapeDict
}