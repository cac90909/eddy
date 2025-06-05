# domain/explorer/operations/enriched_operations.py

from typing import List

from explorer.domain.operations.operation_registry import register_operation
from explorer.domain.operations.base_operation import SQLBaseOperation
from explorer.domain.operations.argument import Argument
from explorer.domain.enums.operation_category import OperationCategory
from explorer.domain.enums.operation_result_type import OperationResultType
from explorer.domain.enums.display_mode import DisplayMode

from shared.services.universal_enriched_service import UniversalEnrichedService
from explorer.util import operation_util

@register_operation
class GroupAggregateOperation(SQLBaseOperation):
    """
    Groups by one or more columns and performs an aggregate (sum, avg, etc.).
    """

    name = "group_aggregate"
    category = OperationCategory.DATA_QUERY
    result_type = OperationResultType.ENRICHED
    http_method = "GET"
    display = DisplayMode.OPERATION_NAVIGATION

    @classmethod
    def arguments(cls) -> List[Argument]:
        return [
            Argument("user_id", int, required=True),

            Argument(
                "group_columns",
                str,
                required=True,
                multiple=True,
                options_fetch=operation_util.get_groupable_columns,
            ),

            Argument(
                "aggregate_operation",
                str,
                required=True,
                options_fetch=operation_util.get_aggregate_operation_options,
                dependency="group_columns",
            ),

            Argument(
                "target_column",
                str,
                required=True,
                options_fetch=operation_util.get_aggregate_target_column_options,
                dependency="group_columns",  # could also depend on aggregate_operation if needed
            ),

            Argument(
                "frequency",
                str,
                required=False,
                options=["daily", "weekly", "monthly", "yearly"],
            ),
        ]

    @staticmethod
    def handler(user_id: int, data_source, **kwargs):
        return UniversalEnrichedService().group_aggregate(user_id=user_id, **kwargs)
