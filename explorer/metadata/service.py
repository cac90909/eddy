# explorer/services/metadata_service.py

from typing import Any, Sequence, Mapping
from shared.operation.enums import OperationType

class ExplorerMetadataService:

    def shape_for_raw(self, result_data: Sequence[Any]) -> dict:
        """
        result_data: list of Django model instances
        """
        return self._package_shape_obj(
            len(result_data),
            len(result_data[0]._meta.fields) if len(result_data) else 0 
        )

    def shape_for_enriched(self, result_data: Sequence[Mapping]) -> dict:
        """
        result_data: list of dicts (post-transformation)
        """
        return self._package_shape_obj(
            len(result_data), 
            len(result_data[0]) if len(result_data) else 0
        )

    def shape_for_metric(self, result_data: Any) -> dict:
        return self._package_shape_obj(1,1)

    def shape_for_list(self, result_data: Sequence[Any]) -> dict:
        """
        result_data: flat list of primitives (strings, ints, etc)
        """
        return self._package_shape_obj(len(result_data), 1)
    
        #NOTE: not in use right now - operation service calls one of the above methods
    #      if 'include_meta'key is included/True
    def compute_data_shape(self, result_data: Any, result_type: OperationType) -> dict:
        """
        Dispatch to the right helper based on result_type.
        result_type might be one of: 'raw', 'enriched', 'metric', 'list'
        """
        if result_type == OperationType.RAW:
            return self.shape_for_raw(result_data)
        elif result_type == OperationType.ENRICHED:
            return self.shape_for_enriched(result_data)
        elif result_type == OperationType.METRIC:
            return self.shape_for_metric(result_data)
        elif result_type == OperationType.LIST:
            return self.shape_for_list(result_data)
        else:
            raise ValueError(f"Unknown result_type: {result_type}")

    def _package_shape_obj(self, num_rows: int, num_columns: int) -> dict:
        return {
            "num_rows": num_rows,
            "num_columns": num_columns,
            "num_values": num_rows*num_columns
        }
    
