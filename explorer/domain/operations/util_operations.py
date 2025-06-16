# domain/explorer/operations/util_operations.py

from typing import List

from explorer.domain.operations.operation_registry import register_operation
from explorer.domain.operations.base_operation import BaseOperation
from explorer.domain.operations.argument import Argument
from explorer.domain.enums.operation_category import OperationCategory
from explorer.domain.enums.operation_result_type import OperationResultType
from explorer.domain.enums.display_mode import DisplayMode

from explorer.cache.service import ExplorerCacheService
from explorer.util import operation_util

@register_operation
class GetOperationChainOperation(BaseOperation):
    name = "get_operation_chain"
    category = OperationCategory.UTILITY
    result_type = OperationResultType.OPERATION_CHAIN
    http_method = "GET"
    display = DisplayMode.NONE

    @classmethod
    def arguments(cls) -> List[Argument]:
        return [Argument("user_id", int, required=True)]

    @staticmethod
    def handler(user_id: int, **kwargs):
        return ExplorerCacheService().get_operation_chain(user_id=user_id)

@register_operation
class GetOperationChainOperationsOperation(BaseOperation):
    name = "get_operation_chain_operations"
    category = OperationCategory.UTILITY
    result_type = OperationResultType.OPERATIONS_LIST
    http_method = "GET"
    display = DisplayMode.NONE

    @classmethod
    def arguments(cls) -> List[Argument]:
        return [Argument("user_id", int, required=True)]

    @staticmethod
    def handler(user_id: int, **kwargs):
        return ExplorerCacheService().extract_operation_chain_operations(user_id=user_id)

@register_operation
class GetOperationChainResultsOperation(BaseOperation):
    name = "get_operation_chain_results"
    category = OperationCategory.UTILITY
    result_type = OperationResultType.RESULTS_LIST
    http_method = "GET"
    display = DisplayMode.NONE

    @classmethod
    def arguments(cls) -> List[Argument]:
        return [Argument("user_id", int, required=True)]

    @staticmethod
    def handler(user_id: int, **kwargs):
        return ExplorerCacheService().extract_operation_chain_result_data(user_id=user_id)

@register_operation
class GetColumnOperatorOptionsOperation(BaseOperation):
    name = "get_column_operator_options"
    category = OperationCategory.UTILITY
    result_type = OperationResultType.LIST
    http_method = "GET"
    display = DisplayMode.NONE

    @classmethod
    def arguments(cls) -> List[Argument]:
        return [Argument("user_id", int, required=True)]

    @staticmethod
    def handler(user_id: int, **kwargs):
        # Depends on current raw dataset; use SQLBaseOperation.data_source semantics
        # But BaseOperation.data_source returns None, so we call util directly
        return operation_util.get_column_operator_options(user_id=user_id)

@register_operation
class GetFilterableColumnNamesOperation(BaseOperation):
    name = "get_filterable_column_names"
    category = OperationCategory.UTILITY
    result_type = OperationResultType.LIST
    http_method = "GET"
    display = DisplayMode.NONE

    @classmethod
    def arguments(cls) -> List[Argument]:
        return [Argument("user_id", int, required=True)]

    @staticmethod
    def handler(user_id: int, **kwargs):
        return operation_util.get_filterable_column_names(user_id=user_id)

@register_operation
class GetOperationTypesOperation(BaseOperation):
    name = "get_operation_types"
    category = OperationCategory.UTILITY
    result_type = OperationResultType.LIST
    http_method = "GET"
    display = DisplayMode.NONE

    @classmethod
    def arguments(cls) -> List[Argument]:
        return [Argument("user_id", int, required=True)]

    @staticmethod
    def handler(user_id: int, **kwargs):
        return operation_util.get_operation_result_data_types(user_id=user_id)

@register_operation
class GetOperationNamesForResultDataTypeOperation(BaseOperation):
    name = "get_operation_names_for_result_data_type"
    category = OperationCategory.UTILITY
    result_type = OperationResultType.LIST
    http_method = "GET"
    display = DisplayMode.NONE

    @classmethod
    def arguments(cls) -> List[Argument]:
        return [
            Argument("user_id", int, required=True),
            Argument("operation_result_data_type", str, required=True),
        ]

    @staticmethod
    def handler(user_id: int, operation_result_data_type: str, **kwargs):
        return operation_util.get_operation_names_for_result_data_type(
            user_id=user_id, operation_result_data_type=operation_result_data_type
        )

@register_operation
class GetOperationArgumentNamesOperation(BaseOperation):
    name = "get_operation_argument_names"
    category = OperationCategory.UTILITY
    result_type = OperationResultType.LIST
    http_method = "GET"
    display = DisplayMode.NONE

    @classmethod
    def arguments(cls) -> List[Argument]:
        return [
            Argument("user_id", int, required=True),
            Argument("operation_name", str, required=True),
        ]

    @staticmethod
    def handler(user_id: int, operation_name: str, **kwargs):
        return operation_util.get_operation_argument_names(
            user_id=user_id, operation_name=operation_name
        )

@register_operation
class GetOperationArgumentOptionsOperation(BaseOperation):
    name = "get_operation_argument_options"
    category = OperationCategory.UTILITY
    result_type = OperationResultType.LIST
    http_method = "GET"
    display = DisplayMode.OPERATION_NAVIGATION

    @classmethod
    def arguments(cls) -> List[Argument]:
        return [
            Argument("user_id", int, required=True),
            Argument("operation_name", str, required=True),
            Argument("operation_argument_name", str, required=True),
            Argument("prev_argument_values", str, required=True),
        ]

    @staticmethod
    def handler(user_id: int, operation_name: str, operation_argument_name: str, prev_argument_values: str, **kwargs):
        return operation_util.get_operation_argument_options(
            user_id=user_id,
            operation_name=operation_name,
            operation_argument_name=operation_argument_name,
            prev_argument_values=prev_argument_values,
        )
