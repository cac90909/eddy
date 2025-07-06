from typing import Any, Optional, Dict
from explorer.infrastructure.explorer_cache import ExplorerCache
from explorer.services.metadata_calculator import ExplorerMetadataCalculator
from core.domain.operation.structures.operation import Operation
from core.infrastructure.cache.cache_resource import CacheResource
from explorer.infrastructure.meta_resource import MetaResource



class ExplorerMetadataManager:
    """
    Application‑level service that manages domain‑specific metadata fields,
    delegating persistence to the MetaCacheManager (flat key approach).
    """
    
    def __init__(self,):
        self.cache = ExplorerCache()

    
    def increment_operation_count(self, user_id: int) -> int:
        key = self.cache.meta_operation_count_key(user_id)
        oc = self.cache.get(key)
        oc = oc + 1
        self.cache.set(key, oc)
        return oc
    







    

    def initialize_metadata(self, user_id, op: Operation) -> None:
        full_shape = curr_shape = self.meta_calc.generate_result_shape(op.result, op.type)
        self.meta_cache.save_field(user_id, self.FULL_SHAPE_KEY, full_shape)
        self.meta_cache.save_field(user_id, self.CURRENT_SHAPE_KEY, curr_shape)
        self.meta_cache.save_field(user_id, self.OPERATION_COUNT_KEY, 1)

    def update_metadata(self, user_id, op: Operation) -> None:
        curr_shape = self.meta_calc.generate_result_shape(op.result, op.type)
        self.meta_cache.save_field(user_id, self.CURRENT_SHAPE_KEY, curr_shape)
        prev_op_count = self.meta_cache.get_field(user_id, self.OPERATION_COUNT_KEY)
        self.meta_cache.save_field(user_id, self.OPERATION_COUNT_KEY, prev_op_count+1)

    def generate_metadata(self, user_id, op: Operation) -> None:
        curr_shape = self.meta_calc.generate_result_shape(op.result, op.type)
        if self.meta_cache.get_field(user_id, self.FULL_SHAPE_KEY) is None:
            full_shape = curr_shape
        else:
            full_shape = self.meta_cache.get_field(user_id, self.FULL_SHAPE_KEY)
        

    def generate_operation_metadata(self, user_id, op: Operation) -> dict:
        shape = self.generate_operation_shape(op.result, op.type)
        op_count = self.cache_svc.get_operation_count(user_id)
        op_count += 1
        return {self.SHAPE: shape, self.OPERATION_COUNT: op_count}

    def set_full_shape(self, user_id: int, shape: dict) -> None:
        """Store the computed full_shape dict."""
        self.cache.save_field(user_id, self.FULL_SHAPE_KEY, shape)

    def get_full_shape(self, user_id: int) -> Optional[dict]:
        """Retrieve the full_shape dict, or None if not set."""
        return self.cache.load_field(user_id, self.FULL_SHAPE_KEY, default=None)

    def clear_full_shape(self, user_id: int) -> None:
        """Reset full_shape to an empty dict."""
        self.cache.save_field(user_id, self.FULL_SHAPE_KEY, {})

    def set_current_shape(self, user_id: int, shape: dict) -> None:
        """Store the computed current_shape dict."""
        self.cache.save_field(user_id, self.CURRENT_SHAPE_KEY, shape)

    def get_current_shape(self, user_id: int) -> Optional[dict]:
        """Retrieve the current_shape dict, or None if not set."""
        return self.cache.load_field(user_id, self.CURRENT_SHAPE_KEY, default=None)

    def clear_current_shape(self, user_id: int) -> None:
        """Reset current_shape to an empty dict."""
        self.cache.save_field(user_id, self.CURRENT_SHAPE_KEY, {})

    # def increment_operation_count(self, user_id: int) -> None:
    #     """Increment the stored operation_count by 1."""
    #     current = self.cache.load_field(user_id, self.OPERATION_COUNT_KEY, default=0)
    #     new_count = (current or 0) + 1
    #     self.cache.save_field(user_id, self.OPERATION_COUNT_KEY, new_count)

    def get_operation_count(self, user_id: int) -> int:
        """Retrieve the number of operations run so far."""
        return self.cache.load_field(user_id, self.OPERATION_COUNT_KEY, default=0)

    def clear_operation_count(self, user_id: int) -> None:
        """Reset the operation count to 1 (as per legacy behavior)."""
        # If you prefer zero, change this to 0
        self.cache.save_field(user_id, self.OPERATION_COUNT_KEY, 1)