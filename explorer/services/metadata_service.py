# explorer/services/metadata_service.py

from typing import Any, Sequence, Mapping

class ExplorerMetadataService:
    """
    Compute row/column/value counts for different kinds of result sets:
      - raw model instances
      - enriched (dict-like) rows
      - single-metric scalars
      - simple primitive lists
    """

    def shape_for_raw(self, result_data: Sequence[Any]) -> dict:
        """
        result_data: list of Django model instances
        """
        num_rows    = len(result_data)
        num_cols    = len(result_data[0]._meta.fields) if num_rows else 0
        num_values  = num_rows * num_cols
        return {
            "num_rows":    num_rows,
            "num_columns": num_cols,
            "num_values":  num_values,
        }

    def shape_for_enriched(self, result_data: Sequence[Mapping]) -> dict:
        """
        result_data: list of dicts (post-transformation)
        """
        num_rows    = len(result_data)
        num_cols    = len(result_data[0]) if num_rows else 0
        num_values  = num_rows * num_cols
        return {
            "num_rows":    num_rows,
            "num_columns": num_cols,
            "num_values":  num_values,
        }

    def shape_for_metric(self, result_data: Any) -> dict:
        """
        result_data: a single scalar (count, avg, sum, etc)
        """
        # always a single cell
        return {
            "num_rows":    1,
            "num_columns": 1,
            "num_values":  1,
        }

    def shape_for_list(self, result_data: Sequence[Any]) -> dict:
        """
        result_data: flat list of primitives (strings, ints, etc)
        """
        num_rows    = len(result_data)
        num_cols    = 1
        num_values  = num_rows  # same as num_rows × num_cols
        return {
            "num_rows":    num_rows,
            "num_columns": num_cols,
            "num_values":  num_values,
        }

    #NOTE: not in use right now - operation service calls one of the above methods
    #      if 'include_meta'key is included/True
    def compute_data_shape(self,
                              result_data: Any,
                              result_type: str) -> dict:
        """
        Dispatch to the right helper based on result_type.
        result_type might be one of: 'raw', 'enriched', 'metric', 'list'
        """
        if result_type == "raw":
            return self.shape_for_raw(result_data)
        elif result_type == "enriched":
            return self.shape_for_enriched(result_data)
        elif result_type == "metric":
            return self.shape_for_metric(result_data)
        elif result_type == "list":
            return self.shape_for_list(result_data)
        else:
            raise ValueError(f"Unknown result_type: {result_type}")
