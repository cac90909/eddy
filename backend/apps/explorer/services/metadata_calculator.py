# explorer/services/metadata_service.py

from typing import Any, Sequence, Mapping
from core.domain.operation.enums import OperationType
from backend.apps.core.domain.operation.structures.operation import Operation
from explorer.domain.structures.metadata import OperationMetadata
from explorer.domain.structures.data_shape import DataShape
from datetime import datetime

class ExplorerMetadataCalculator:
    
    def increment_operation_count(self, metadata: OperationMetadata) -> int:
        return metadata.operation_count + 1
    
    def get_time_string(self) -> str:
        return str(datetime.now())
    
    def generate_result_shape(self, result_data, result_type) -> DataShape:
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
        else:
            raise Exception("Invalid Datatype")

    def _package_shape_obj(self, num_rows: int, num_columns: int) -> DataShape:
        return DataShape(
            num_rows = num_rows, 
            num_cols=num_columns, 
            num_vals=num_rows*num_columns
        )

    
