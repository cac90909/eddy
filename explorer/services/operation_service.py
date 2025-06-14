# explorer/services/eoperation_service.py

from typing import Any, Callable, Dict

from shared.services.universal_raw_service import UniversalRawService
from shared.services.universal_list_service import UniversalListService
from shared.services.universal_metric_service import UniversalMetricService
from shared.services.universal_enriched_service import UniversalEnrichedService
from rest_framework.exceptions import NotFound

from explorer.domain.operation import Operation


class OperationNotFoundError(Exception):
    pass


class ExplorerOperationService:
    """
    Central service that offers one method per public operation,
    delegates to the appropriate core service, and maintains
    a registry for dynamic dispatch (e.g. chain replay).
    """

    def __init__(self):
        self.raw_svc      = UniversalRawService()
        self.list_svc     = UniversalListService()
        self.metric_svc   = UniversalMetricService()
        self.enriched_svc = UniversalEnrichedService()

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

    # ─── Individual operation methods ───────────────────────────────────────────

    def get_full_data(self, user_id: int, **kwargs) -> Any:
        return self.raw_svc.get_full_data(user_id=user_id)

    def filter(self, user_id: int, previous: Any = None, **kwargs) -> Any:
        return self.raw_svc.filter(
            user_id=user_id,
            data_source=previous,
            **kwargs
        )

    def traverse(self, user_id: int, previous: Any = None, **kwargs) -> Any:
        return self.raw_svc.traverse(
            user_id=user_id,
            data_source=previous,
            **kwargs
        )

    def get_unique_column_values(self, user_id: int, previous: Any = None, **kwargs) -> Any:
        return self.list_svc.get_unique_column_values(
            user_id=user_id,
            data_source=previous,
            **kwargs
        )

    def get_unique_json_keys(self, user_id: int, previous: Any = None, **kwargs) -> Any:
        return self.list_svc.get_unique_json_keys(
            user_id=user_id,
            data_source=previous,
            **kwargs
        )

    def get_unique_json_values(self, user_id: int, previous: Any = None, **kwargs) -> Any:
        return self.list_svc.get_unique_json_values(
            user_id=user_id,
            data_source=previous,
            **kwargs
        )

    def get_unique_json_key_values(self, user_id: int, previous: Any = None, **kwargs) -> Any:
        return self.list_svc.get_unique_json_key_values(
            user_id=user_id,
            data_source=previous,
            **kwargs
        )

    def get_count(self, user_id: int, previous: Any = None, **kwargs) -> Any:
        return self.metric_svc.get_count(
            user_id=user_id,
            data_source=previous,
            **kwargs
        )

    def get_average(self, user_id: int, previous: Any = None, **kwargs) -> Any:
        return self.metric_svc.get_average(
            user_id=user_id,
            data_source=previous,
            **kwargs
        )

    def get_sum(self, user_id: int, previous: Any = None, **kwargs) -> Any:
        return self.metric_svc.get_sum(
            user_id=user_id,
            data_source=previous,
            **kwargs
        )

    def get_min(self, user_id: int, previous: Any = None, **kwargs) -> Any:
        return self.metric_svc.get_min(
            user_id=user_id,
            data_source=previous,
            **kwargs
        )

    def get_max(self, user_id: int, previous: Any = None, **kwargs) -> Any:
        return self.metric_svc.get_max(
            user_id=user_id,
            data_source=previous,
            **kwargs
        )

    def group_aggregate(self, user_id: int, previous: Any = None, **kwargs) -> Any:
        return self.enriched_svc.group_aggregate(
            user_id=user_id,
            data_source=previous,
            **kwargs
        )
