from explorer.infrastructure.explorer_cache import ExplorerCache
from explorer.services.metadata_calculator import ExplorerMetadataCalculator
from core.domain.operation.structures.operation import Operation
from explorer.domain.structures.metadata import OperationMetadata
from explorer.domain.structures.data_shape import DataShape



class ExplorerMetadataManager:
    """
    Application‑level service that manages domain‑specific metadata fields,
    delegating persistence to the MetaCacheManager (flat key approach).
    """
    
    def __init__(self,):
        self.meta_cache = ExplorerCache().meta
        self.meta_calc = ExplorerMetadataCalculator()

    def _get_metadata(self, user_id: int) -> OperationMetadata:
        return self.meta_cache.get(user_id)
    
    def _set_metadata(self, user_id: int, metadata: OperationMetadata) -> None:
        self.meta_cache.set(user_id, metadata)

    def _clear_metadata(self, user_id) -> None:
        self.meta_cache.clear(user_id)

    def initialize_metadata(self, user_id, op: Operation) -> OperationMetadata:
        init_meta = self._get_metadata(user_id)
        init_meta.operation_count = 1
        init_meta.session_start_time = init_meta.last_updated = self.meta_calc.get_time_string()
        init_meta.full_shape = init_meta.current_shape = self.meta_calc.generate_result_shape(op.result, op.type)
        self._set_metadata(user_id, init_meta)
        return init_meta

    def update_metadata(self, user_id: int, op: Operation) -> OperationMetadata:
        updated_meta = self._get_metadata(user_id)
        updated_meta.operation_count = self.meta_calc.increment_operation_count(updated_meta)
        updated_meta.last_updated = self.meta_calc.get_time_string()
        updated_meta.current_shape = self.meta_calc.generate_result_shape(op.result, op.type)
        self._set_metadata(user_id, updated_meta)
        return updated_meta
    
    def reset_metadata(self, user_id) -> OperationMetadata:
        reset_meta = self._get_metadata(user_id)
        reset_meta.operation_count = 1
        reset_meta.last_updated = self.meta_calc.get_time_string()
        reset_meta.full_shape = reset_meta.current_shape = DataShape()
        self._set_metadata(user_id, reset_meta)
        return reset_meta
    
    def delete_metadata(self, user_id) -> None:
        self.meta_cache.delete(user_id)

