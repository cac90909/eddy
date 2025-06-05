# domain/explorer/operations/raw_operations.py

from typing import List

from explorer.domain.operations.operation_registry import register_operation
from explorer.domain.operations.base_operation import SQLBaseOperation
from explorer.domain.operations.argument import Argument
from explorer.domain.enums.operation_category import OperationCategory
from explorer.domain.enums.operation_result_type import OperationResultType
from explorer.domain.enums.display_mode import DisplayMode

from explorer.services.explorer_cache_service import ExplorerCacheService
from shared.services.universal_raw_service import UniversalRawService
from shared.services.universal_list_service import UniversalListService
from explorer.util import operation_util

@register_operation
class GetFullDataOperation(SQLBaseOperation):
    """
    Returns the user’s entire raw dataset with no filtering.
    """

    name = "get_full_data"
    # category inherited as OperationCategory.DATA_QUERY
    result_type = OperationResultType.RAW
    http_method = "GET"
    display = None

    @classmethod
    def arguments(cls) -> List[Argument]:
        return [
            Argument("user_id", int, required=True),
        ]

    @staticmethod
    def handler(user_id: int, **kwargs):
        return UniversalRawService().get_full_data(user_id=user_id, **kwargs)

    @staticmethod
    def data_source(user_id: int, op_instance: "GetFullDataOperation"):
        # Override to return None
        return None

@register_operation
class FilterOperation(SQLBaseOperation):
    """
    Applies a filter to the most-recently cached dataset chain.
    """

    name = "filter"
    # category inherited as OperationCategory.DATA_QUERY
    result_type = OperationResultType.RAW
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

            Argument(
                "filter_value",
                str,
                required=True,
                options_fetch=UniversalListService().get_unique_column_values,
                dependency="column_name",
            ),

            Argument(
                "filter_type",
                str,
                required=True,
                options_fetch=operation_util.get_column_operator_options,
                dependency="column_name",
            ),
        ]

    @staticmethod
    def handler(user_id: int, data_source, column_name: str, filter_value: str, filter_type: str, **kwargs):
        return UniversalRawService().filter(
            user_id=user_id,
            data_source=data_source,
            column_name=column_name,
            filter_value=filter_value,
            filter_type=filter_type,
        )

@register_operation
class TraverseOperation(SQLBaseOperation):
    """
    Walks links in the user’s dataset (e.g. parent/child relationships).
    """

    name = "traverse"
    # category inherited as OperationCategory.DATA_QUERY
    result_type = OperationResultType.RAW
    http_method = "GET"
    display = DisplayMode.OPERATION_NAVIGATION

    @classmethod
    def arguments(cls) -> List[Argument]:
        return [
            Argument("user_id", int, required=True),

            Argument(
                "start_id",
                int,
                required=True,
                options_fetch=UniversalListService().get_unique_column_values,
                dependency="entry_id",
            ),

            Argument(
                "traversal_directions",
                list,
                required=True,
                multiple=True,
                options=["horizontal", "upwards", "downwards"],
            ),
        ]

    @staticmethod
    def handler(user_id: int, data_source, start_id: int, traversal_directions: List[str], **kwargs):
        return UniversalRawService().traverse(
            user_id=user_id,
            data_source=data_source,
            start_id=start_id,
            traversal_directions=traversal_directions,
        )
