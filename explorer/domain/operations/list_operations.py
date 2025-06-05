# domain/explorer/operations/list_operations.py

from typing import List

from explorer.domain.operations.operation_registry import register_operation
from explorer.domain.operations.base_operation import SQLBaseOperation
from explorer.domain.operations.argument import Argument
from explorer.domain.enums.operation_category import OperationCategory
from explorer.domain.enums.operation_result_type import OperationResultType
from explorer.domain.enums.display_mode import DisplayMode

from shared.services.universal_list_service import UniversalListService
from explorer.util import operation_util

@register_operation
class GetUniqueColumnValuesOperation(SQLBaseOperation):
    name = "get_unique_column_values"
    category = OperationCategory.UTILITY
    result_type = OperationResultType.LIST
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
        return UniversalListService().get_unique_column_values(
            user_id=user_id, column_name=column_name
        )

@register_operation
class GetUniqueJsonKeysOperation(SQLBaseOperation):
    name = "get_unique_json_keys"
    category = OperationCategory.UTILITY
    result_type = OperationResultType.LIST
    http_method = "GET"
    display = DisplayMode.OPERATION_NAVIGATION

    @classmethod
    def arguments(cls) -> List[Argument]:
        return [Argument("user_id", int, required=True)]

    @staticmethod
    def handler(user_id: int, data_source, **kwargs):
        return UniversalListService().get_unique_json_keys(user_id=user_id)

@register_operation
class GetUniqueJsonValuesOperation(SQLBaseOperation):
    name = "get_unique_json_values"
    category = OperationCategory.UTILITY
    result_type = OperationResultType.LIST
    http_method = "GET"
    display = DisplayMode.OPERATION_NAVIGATION

    @classmethod
    def arguments(cls) -> List[Argument]:
        return [Argument("user_id", int, required=True)]

    @staticmethod
    def handler(user_id: int, data_source, **kwargs):
        return UniversalListService().get_unique_json_values(user_id=user_id)

@register_operation
class GetUniqueJsonKeyValuesOperation(SQLBaseOperation):
    name = "get_unique_json_key_values"
    category = OperationCategory.UTILITY
    result_type = OperationResultType.LIST
    http_method = "GET"
    display = DisplayMode.OPERATION_NAVIGATION

    @classmethod
    def arguments(cls) -> List[Argument]:
        return [
            Argument("user_id", int, required=True),
            Argument("json_key", str, required=True),
        ]

    @staticmethod
    def handler(user_id: int, data_source, json_key: str, **kwargs):
        return UniversalListService().get_unique_json_key_values(
            user_id=user_id, json_key=json_key
        )

@register_operation
class GetUniqueColumnValuesFilterOptionsOperation(SQLBaseOperation):
    """
    Used only to populate “filter” dropdown options; not shown in main UI.
    """

    name = "get_unique_column_values_filter_options"
    category = OperationCategory.UTILITY
    result_type = OperationResultType.LIST
    http_method = "GET"
    display = DisplayMode.NONE  # no direct UI control

    @classmethod
    def arguments(cls) -> List[Argument]:
        return [
            Argument("user_id", int, required=True),
            Argument("column_name", str, required=True),
        ]

    @staticmethod
    def handler(user_id: int, data_source, column_name: str, **kwargs):
        return UniversalListService().get_unique_column_values(
            user_id=user_id, column_name=column_name
        )

    @staticmethod
    def cache_policy(op_instance) -> bool:
        return False  # “filter options” should not be cached

@register_operation
class GetUniqueJsonKeysFilterOptionsOperation(SQLBaseOperation):
    name = "get_unique_json_keys_filter_options"
    category = OperationCategory.UTILITY
    result_type = OperationResultType.LIST
    http_method = "GET"
    display = DisplayMode.NONE

    @classmethod
    def arguments(cls) -> List[Argument]:
        return [Argument("user_id", int, required=True)]

    @staticmethod
    def handler(user_id: int, data_source, **kwargs):
        return UniversalListService().get_unique_json_keys(user_id=user_id)

    @staticmethod
    def cache_policy(op_instance) -> bool:
        return False

@register_operation
class GetUniqueJsonValuesFilterOptionsOperation(SQLBaseOperation):
    name = "get_unique_json_values_filter_options"
    category = OperationCategory.UTILITY
    result_type = OperationResultType.LIST
    http_method = "GET"
    display = DisplayMode.NONE

    @classmethod
    def arguments(cls) -> List[Argument]:
        return [Argument("user_id", int, required=True)]

    @staticmethod
    def handler(user_id: int, data_source, **kwargs):
        return UniversalListService().get_unique_json_values(user_id=user_id)

    @staticmethod
    def cache_policy(op_instance) -> bool:
        return False

@register_operation
class GetUniqueJsonKeyValuesFilterOptionsOperation(SQLBaseOperation):
    name = "get_unique_json_key_values_filter_options"
    category = OperationCategory.UTILITY
    result_type = OperationResultType.LIST
    http_method = "GET"
    display = DisplayMode.NONE

    @classmethod
    def arguments(cls) -> List[Argument]:
        return [
            Argument("user_id", int, required=True),
            Argument("json_key", str, required=True),
        ]

    @staticmethod
    def handler(user_id: int, data_source, json_key: str, **kwargs):
        return UniversalListService().get_unique_json_key_values(
            user_id=user_id, json_key=json_key
        )

    @staticmethod
    def cache_policy(op_instance) -> bool:
        return False
