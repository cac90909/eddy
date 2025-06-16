# explorer/services/eoperation_service.py
from typing import Any, Dict, Callable

from rest_framework.exceptions import APIException, NotFound
from shared.services.universal_raw_service import UniversalRawService
from shared.services.universal_list_service import UniversalListService
from shared.services.universal_metric_service import UniversalMetricService
from shared.services.universal_enriched_service import UniversalEnrichedService
from explorer.cache.service import ExplorerCacheService
from explorer.metadata.service import ExplorerMetadataService
from explorer.domain.operation import Operation


class ExplorerOperationService:
    """
    Service layer that wraps core Universal* services,
    caches each operation and its result for undo/replay,
    and uniformly handles exceptions.
    """

    def __init__(self):
        self.raw_svc      = UniversalRawService()
        self.list_svc     = UniversalListService()
        self.metric_svc   = UniversalMetricService()
        self.enriched_svc = UniversalEnrichedService()
        self.cache_svc    = ExplorerCacheService()
        self.metadata_svc = ExplorerMetadataService()

        #TODO - possible to not hard code? Or maybe save as enum?
        self._registry: Dict[str, Callable[..., Any]] = {
            "get_full_data":               self.get_full_data,
            "filter":                      self.filter,
            "traverse":                    self.traverse,
            "get_unique_column_values":    self.get_unique_column_values,
            "get_unique_json_keys":        self.get_unique_json_keys,
            "get_unique_json_values":      self.get_unique_json_values,
            "get_unique_json_key_values":  self.get_unique_json_key_values,
            "get_count":                   self.get_count,
            "get_average":                 self.get_average,
            "get_sum":                     self.get_sum,
            "get_min":                     self.get_min,
            "get_max":                     self.get_max,
            "group_aggregate":             self.group_aggregate,
        }

    def execute(
        self,
        user_id: int,
        operation_name: str,
        operation_args: dict,
        previous: Any = None
    ) -> Any:
        """
        Dynamic dispatch for replaying a chain:
        looks up by operation_name and invokes it.
        """
        fn = self._registry.get(operation_name)
        if not fn:
            raise NotFound(f"Operation '{operation_name}' is not registered.")
        # pass 'previous' only if fn signature expects it; here
        return fn(user_id=user_id, previous=previous, **operation_args)

    def get_full_data(self, user_id: int, **kwargs) -> Any:
        try:
            previous = self.cache_svc.last_result(user_id)
            result = self.raw_svc.get_full_data(user_id=user_id)
            entry = Operation(
                name="get_full_data",
                args={},
                result_type="raw",
                result_data=result
            )
            self.cache_svc.append_operation(user_id, entry)
            shape = self.metadata_svc.compute_data_shape(result, "raw")
            self.cache_svc.set_current_shape(user_id, shape)
            self.cache_svc.increment_operation_count(user_id)
            return result
        except Exception as e:
            raise APIException(str(e))

    def filter(self, user_id: int, previous: Any = None, **kwargs) -> Any:
        try:
            previous = self.cache_svc.last_result(user_id)
            result = self.raw_svc.filter(
                user_id=user_id,
                data_source=previous,
                **kwargs
            )
            entry = Operation(
                name="filter",
                args=kwargs,
                result_type="raw",
                result_data=result
            )
            self.cache_svc.append_operation(user_id, entry)
            shape = self.metadata_svc.compute_data_shape(result, "raw")
            self.cache_svc.set_current_shape(user_id, shape)
            self.cache_svc.increment_operation_count(user_id)
            return result
        except Exception as e:
            raise APIException(str(e))

    def traverse(self, user_id: int, previous: Any = None, **kwargs) -> Any:
        try:
            previous = self.cache_svc.last_result(user_id)
            result = self.raw_svc.traverse(
                user_id=user_id,
                data_source=previous,
                **kwargs
            )
            entry = Operation(
                name="traverse",
                args=kwargs,
                result_type="raw",
                result_data=result
            )
            self.cache_svc.append_operation(user_id, entry)
            shape = self.metadata_svc.compute_data_shape(result, "raw")
            self.cache_svc.set_current_shape(user_id, shape)
            self.cache_svc.increment_operation_count(user_id)
            return result
        except Exception as e:
            raise APIException(str(e))

    def get_unique_column_values(self, user_id: int, previous: Any = None, **kwargs) -> Any:
        try:
            previous = self.cache_svc.last_result(user_id)
            result = self.list_svc.get_unique_column_values(
                user_id=user_id,
                data_source=previous,
                **kwargs
            )
            entry = Operation(
                name="get_unique_column_values",
                args=kwargs,
                result_type="list",
                result_data=result
            )
            self.cache_svc.append_operation(user_id, entry)
            shape = self.metadata_svc.compute_data_shape(result, "list")
            self.cache_svc.set_current_shape(user_id, shape)
            self.cache_svc.increment_operation_count(user_id)
            return result
        except Exception as e:
            raise APIException(str(e))

    def get_unique_json_keys(self, user_id: int, previous: Any = None, **kwargs) -> Any:
        try:
            previous = self.cache_svc.last_result(user_id)
            result = self.list_svc.get_unique_json_keys(
                user_id=user_id,
                data_source=previous,
                **kwargs
            )
            entry = Operation(
                name="get_unique_json_keys",
                args=kwargs,
                result_type="list",
                result_data=result
            )
            self.cache_svc.append_operation(user_id, entry)
            shape = self.metadata_svc.compute_data_shape(result, "list")
            self.cache_svc.set_current_shape(user_id, shape)
            self.cache_svc.increment_operation_count(user_id)
            return result
        except Exception as e:
            raise APIException(str(e))

    def get_unique_json_values(self, user_id: int, previous: Any = None, **kwargs) -> Any:
        try:
            previous = self.cache_svc.last_result(user_id)
            result = self.list_svc.get_unique_json_values(
                user_id=user_id,
                data_source=previous,
                **kwargs
            )
            entry = Operation(
                name="get_unique_json_values",
                args=kwargs,
                result_type="list",
                result_data=result
            )
            self.cache_svc.append_operation(user_id, entry)
            shape = self.metadata_svc.compute_data_shape(result, "list")
            self.cache_svc.set_current_shape(user_id, shape)
            self.cache_svc.increment_operation_count(user_id)
            return result
        except Exception as e:
            raise APIException(str(e))

    def get_unique_json_key_values(self, user_id: int, previous: Any = None, **kwargs) -> Any:
        try:
            previous = self.cache_svc.last_result(user_id)
            result = self.list_svc.get_unique_json_key_values(
                user_id=user_id,
                data_source=previous,
                **kwargs
            )
            entry = Operation(
                name="get_unique_json_key_values",
                args=kwargs,
                result_type="list",
                result_data=result
            )
            self.cache_svc.append_operation(user_id, entry)
            shape = self.metadata_svc.compute_data_shape(result, "list")
            self.cache_svc.set_current_shape(user_id, shape)
            self.cache_svc.increment_operation_count(user_id)
            return result
        except Exception as e:
            raise APIException(str(e))

    def get_count(self, user_id: int, previous: Any = None, **kwargs) -> Any:
        try:
            previous = self.cache_svc.last_result(user_id)
            result = self.metric_svc.get_count(
                user_id=user_id,
                data_source=previous,
                **kwargs
            )
            entry = Operation(
                name="get_count",
                args=kwargs,
                result_type="metric",
                result_data=result
            )
            self.cache_svc.append_operation(user_id, entry)
            shape = self.metadata_svc.compute_data_shape(result, "metric")
            self.cache_svc.set_current_shape(user_id, shape)
            self.cache_svc.increment_operation_count(user_id)
            return result
        except Exception as e:
            raise APIException(str(e))

    def get_average(self, user_id: int, previous: Any = None, **kwargs) -> Any:
        try:
            previous = self.cache_svc.last_result(user_id)
            result = self.metric_svc.get_average(
                user_id=user_id,
                data_source=previous,
                **kwargs
            )
            entry = Operation(
                name="get_average",
                args=kwargs,
                result_type="metric",
                result_data=result
            )
            self.cache_svc.append_operation(user_id, entry)
            shape = self.metadata_svc.compute_data_shape(result, "metric")
            self.cache_svc.set_current_shape(user_id, shape)
            self.cache_svc.increment_operation_count(user_id)
            return result
        except Exception as e:
            raise APIException(str(e))

    def get_sum(self, user_id: int, previous: Any = None, **kwargs) -> Any:
        try:
            previous = self.cache_svc.last_result(user_id)
            result = self.metric_svc.get_sum(
                user_id=user_id,
                data_source=previous,
                **kwargs
            )
            entry = Operation(
                name="get_sum",
                args=kwargs,
                result_type="metric",
                result_data=result
            )
            self.cache_svc.append_operation(user_id, entry)
            shape = self.metadata_svc.compute_data_shape(result, "metric")
            self.cache_svc.set_current_shape(user_id, shape)
            self.cache_svc.increment_operation_count(user_id)
            return result
        except Exception as e:
            raise APIException(str(e))

    def get_min(self, user_id: int, previous: Any = None, **kwargs) -> Any:
        try:
            previous = self.cache_svc.last_result(user_id)
            result = self.metric_svc.get_min(
                user_id=user_id,
                data_source=previous,
                **kwargs
            )
            entry = Operation(
                name="get_min",
                args=kwargs,
                result_type="metric",
                result_data=result
            )
            self.cache_svc.append_operation(user_id, entry)
            shape = self.metadata_svc.compute_data_shape(result, "metric")
            self.cache_svc.set_current_shape(user_id, shape)
            self.cache_svc.increment_operation_count(user_id)
            return result
        except Exception as e:
            raise APIException(str(e))

    def get_max(self, user_id: int, previous: Any = None, **kwargs) -> Any:
        try:
            previous = self.cache_svc.last_result(user_id)
            result = self.metric_svc.get_max(
                user_id=user_id,
                data_source=previous,
                **kwargs
            )
            entry = Operation(
                name="get_max",
                args=kwargs,
                result_type="metric",
                result_data=result
            )
            self.cache_svc.append_operation(user_id, entry)
            shape = self.metadata_svc.compute_data_shape(result, "metric")
            self.cache_svc.set_current_shape(user_id, shape)
            self.cache_svc.increment_operation_count(user_id)
            return result
        except Exception as e:
            raise APIException(str(e))

    def group_aggregate(self, user_id: int, previous: Any = None, **kwargs) -> Any:
        try:
            previous = self.cache_svc.last_result(user_id)
            result = self.enriched_svc.group_aggregate(
                user_id=user_id,
                data_source=previous,
                **kwargs
            )
            entry = Operation(
                name="group_aggregate",
                args=kwargs,
                result_type="enriched",
                result_data=result
            )
            self.cache_svc.append_operation(user_id, entry)
            shape = self.metadata_svc.compute_data_shape(result, "metric")
            self.cache_svc.set_current_shape(user_id, shape)
            self.cache_svc.increment_operation_count(user_id)
            return result
        except Exception as e:
            raise APIException(str(e))
