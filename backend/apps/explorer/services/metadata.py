# explorer/services/metadata_service.py

from typing import Any, Sequence, Mapping
from core.domain.enums.operation import OperationType
from core.domain.operation import Operation
from explorer.services.cache import ExplorerCacheService

class ExplorerMetadataService:

    SHAPE = "shape"
    OPERATION_COUNT = "operation_count"

    def __init__(self):
        self.cache_svc = ExplorerCacheService()

    def generate_operation_metadata(self, user_id, op: Operation) -> dict:
        shape = self.generate_operation_shape(op.result, op.type)
        op_count = self.cache_svc.get_operation_count(user_id)
        op_count += 1
        return {self.SHAPE: shape, self.OPERATION_COUNT: op_count}
    
    def generate_operation_shape(self, result_data, result_type):
        if result_type == OperationType.RAW:
            return self._package_shape_obj(
            len(result_data),
            len(result_data[0]._meta.fields) if len(result_data) else 0 
        )
        elif result_type == OperationType.ENRICHED:
            return self._package_shape_obj(
            len(result_data), 
            len(result_data[0]) if len(result_data) else 0
        )
        elif result_type == OperationType.METRIC:
            return self._package_shape_obj(1,1)
        elif result_type == OperationType.LIST:
            return self._package_shape_obj(len(result_data), 1)

    def _package_shape_obj(self, num_rows: int, num_columns: int) -> dict:
        return {
            "num_rows": num_rows,
            "num_columns": num_columns,
            "num_values": num_rows*num_columns
        }
    
    
