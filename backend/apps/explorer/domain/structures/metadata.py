from dataclasses import dataclass
from typing import Optional
from core.domain.common.structures.base_metadata import BaseMetadata
from explorer.domain.structures.data_shape import DataShape

@dataclass
class OperationMetadata(BaseMetadata):
    last_updated: str        = "0"
    operation_count: int     = 0
    current_shape: DataShape = DataShape()
    full_shape: DataShape    = DataShape()