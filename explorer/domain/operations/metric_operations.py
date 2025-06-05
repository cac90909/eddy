# domain/explorer/operations/metric_operations.py

from typing import List

from explorer.domain.operations.operation_registry import register_operation
from explorer.domain.operations.base_operation import SQLBaseOperation
from explorer.domain.operations.argument import Argument
from explorer.domain.enums.operation_category import OperationCategory
from explorer.domain.enums.operation_result_type import OperationResultType
from explorer.domain.enums.display_mode import DisplayMode

from shared.services.universal_metric_service import UniversalMetricService
from explorer.util import operation_util

@register_operation
class GetCountOperation(SQLBaseOperation):
    name = "get_count"
    category = OperationCategory.METRIC
    result_type = OperationResultType.METRIC
    http_method = "GET"
    display = DisplayMode.OPERATION_NAVIGATION

    @classmethod
    def arguments(cls) -> List[Argument]:
        return [
            Argument("user_id", int, required=True),
            Argument(
                "column_name",
                str,
                required=True,
                options_fetch=operation_util.get_filterable_column_names,
            ),
        ]

    @staticmethod
    def handler(user_id: int, data_source, column_name: str, **kwargs):
        return UniversalMetricService().get_count(
            user_id=user_id, column_name=column_name
        )

@register_operation
class GetAverageOperation(SQLBaseOperation):
    name = "get_average"
    category = OperationCategory.METRIC
    result_type = OperationResultType.METRIC
    http_method = "GET"
    display = DisplayMode.OPERATION_NAVIGATION

    @classmethod
    def arguments(cls) -> List[Argument]:
        return [
            Argument("user_id", int, required=True),
            Argument(
                "column_name",
                str,
                required=True,
                options_fetch=operation_util.get_filterable_column_names,
            ),
        ]

    @staticmethod
    def handler(user_id: int, data_source, column_name: str, **kwargs):
        return UniversalMetricService().get_average(
            user_id=user_id, column_name=column_name
        )

@register_operation
class GetSumOperation(SQLBaseOperation):
    name = "get_sum"
    category = OperationCategory.METRIC
    result_type = OperationResultType.METRIC
    http_method = "GET"
    display = DisplayMode.OPERATION_NAVIGATION

    @classmethod
    def arguments(cls) -> List[Argument]:
        return [
            Argument("user_id", int, required=True),
            Argument(
                "column_name",
                str,
                required=True,
                options_fetch=operation_util.get_filterable_column_names,
            ),
        ]

    @staticmethod
    def handler(user_id: int, data_source, column_name: str, **kwargs):
        return UniversalMetricService().get_sum(
            user_id=user_id, column_name=column_name
        )

@register_operation
class GetMinOperation(SQLBaseOperation):
    name = "get_min"
    category = OperationCategory.METRIC
    result_type = OperationResultType.METRIC
    http_method = "GET"
    display = DisplayMode.OPERATION_NAVIGATION

    @classmethod
    def arguments(cls) -> List[Argument]:
        return [
            Argument("user_id", int, required=True),
            Argument(
                "column_name",
                str,
                required=True,
                options_fetch=operation_util.get_filterable_column_names,
            ),
        ]

    @staticmethod
    def handler(user_id: int, data_source, column_name: str, **kwargs):
        return UniversalMetricService().get_min(
            user_id=user_id, column_name=column_name
        )

@register_operation
class GetMaxOperation(SQLBaseOperation):
    name = "get_max"
    category = OperationCategory.METRIC
    result_type = OperationResultType.METRIC
    http_method = "GET"
    display = DisplayMode.OPERATION_NAVIGATION

    @classmethod
    def arguments(cls) -> List[Argument]:
        return [
            Argument("user_id", int, required=True),
            Argument(
                "column_name",
                str,
                required=True,
                options_fetch=operation_util.get_filterable_column_names,
            ),
        ]

    @staticmethod
    def handler(user_id: int, data_source, column_name: str, **kwargs):
        return UniversalMetricService().get_max(
            user_id=user_id, column_name=column_name
        )
