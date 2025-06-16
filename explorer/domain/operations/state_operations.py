# domain/explorer/operations/state_operations.py

from typing import List

from explorer.domain.operations.operation_registry import register_operation
from explorer.domain.operations.base_operation import BaseOperation
from explorer.domain.operations.argument import Argument
from explorer.domain.enums.operation_category import OperationCategory
from explorer.domain.enums.operation_result_type import OperationResultType
from explorer.domain.enums.display_mode import DisplayMode

from explorer.cache.service import ExplorerCacheService
from shared.services.snapshots_service import SnapshotsService
from shared.services.universal_raw_service import UniversalRawService
from explorer.util import operation_util

@register_operation
class StartExplorerSessionOperation(BaseOperation):
    name = "start_explorer_session"
    category = OperationCategory.SESSION_STATE
    result_type = OperationResultType.CONFIG
    http_method = "GET"
    display = DisplayMode.NONE

    @classmethod
    def arguments(cls) -> List[Argument]:
        return [Argument("user_id", int, required=True)]

    @staticmethod
    def setup(user_id: int, op_instance: "StartExplorerSessionOperation"):
        ExplorerCacheService().create_empty_operation_chain_cache(user_id=user_id)

    @staticmethod
    def handler(user_id: int, **kwargs):
        return operation_util.assemble_operations_config(user_id=user_id)

@register_operation
class ResetExplorerSessionOperation(BaseOperation):
    name = "reset_explorer_session"
    category = OperationCategory.SESSION_STATE
    result_type = OperationResultType.RAW
    http_method = "GET"
    display = DisplayMode.RESET_BUTTON

    @classmethod
    def arguments(cls) -> List[Argument]:
        return [Argument("user_id", int, required=True)]

    @staticmethod
    def setup(user_id: int, op_instance: "ResetExplorerSessionOperation"):
        ExplorerCacheService().create_empty_operation_chain_cache(user_id=user_id)

    @staticmethod
    def handler(user_id: int, **kwargs):
        return UniversalRawService().get_full_data(user_id=user_id)

@register_operation
class UndoOperation(BaseOperation):
    name = "undo_operation"
    category = OperationCategory.SESSION_STATE
    result_type = OperationResultType.RAW
    http_method = "POST"
    display = DisplayMode.UNDO_BUTTON

    @classmethod
    def arguments(cls) -> List[Argument]:
        return [Argument("user_id", int, required=True)]

    @staticmethod
    def precondition(user_id: int):
        chain = ExplorerCacheService().get_operation_chain(user_id=user_id)
        if len(chain) <= 1:
            raise Exception("Cannot undo operation: no operations to undo")

    @staticmethod
    def setup(user_id: int, op_instance: "UndoOperation"):
        ExplorerCacheService().delete_most_recent_operation_from_chain(user_id=user_id)

    @staticmethod
    def handler(user_id: int, **kwargs):
        return ExplorerCacheService().get_most_recent_operation_chain_raw_data_result(user_id=user_id)

@register_operation
class LoadSnapshotOperation(BaseOperation):
    name = "load_snapshot"
    category = OperationCategory.SESSION_STATE
    result_type = OperationResultType.RAW
    http_method = "GET"
    display = DisplayMode.LOAD_BUTTON

    @classmethod
    def arguments(cls) -> List[Argument]:
        return [
            Argument("user_id", int, required=True),
            Argument("snapshot_id", str, required=True),
        ]

    @staticmethod
    def setup(user_id: int, op_instance: "LoadSnapshotOperation"):
        ExplorerCacheService().empty_operation_chain(user_id=user_id)

    @staticmethod
    def data_source(user_id: int, op_instance: "LoadSnapshotOperation"):
        # Digs the chain out of the snapshot
        snapshot_id = op_instance.operation_arguments["snapshot_id"]
        return SnapshotsService().get_snapshot_operation_chain(
            user_id=user_id, snapshot_id=snapshot_id
        )

    @staticmethod
    def handler(user_id: int, data_source, **kwargs):
        return operation_util.assemble_dataset_list_from_operation_chain(
            user_id=user_id, operation_chain=data_source
        )

@register_operation
class EndExplorerSessionOperation(BaseOperation):
    name = "end_explorer_session"
    category = OperationCategory.SESSION_STATE
    result_type = OperationResultType.STATUS
    http_method = "POST"
    display = DisplayMode.NONE

    @classmethod
    def arguments(cls) -> List[Argument]:
        return [Argument("user_id", int, required=True)]

    @staticmethod
    def handler(user_id: int, **kwargs):
        return ExplorerCacheService().delete_operation_chain_cache(user_id=user_id)
