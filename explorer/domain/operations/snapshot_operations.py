# domain/explorer/operations/snapshot_operations.py

from typing import List

from explorer.domain.operations.operation_registry import register_operation
from explorer.domain.operations.base_operation import BaseOperation
from explorer.domain.operations.argument import Argument
from explorer.domain.enums.operation_category import OperationCategory
from explorer.domain.enums.operation_result_type import OperationResultType
from explorer.domain.enums.display_mode import DisplayMode

from shared.services.snapshots_service import SnapshotsService
from explorer.cache.service import ExplorerCacheService

@register_operation
class SaveSnapshotOperation(BaseOperation):
    name = "save_snapshot"
    category = OperationCategory.SNAPSHOT
    result_type = OperationResultType.SNAPSHOT
    http_method = "POST"
    display = DisplayMode.SAVE_BUTTON

    @classmethod
    def arguments(cls) -> List[Argument]:
        return [
            Argument("user_id", int, required=True),
            Argument("title", str, required=True),
            Argument("description", str, required=False),
        ]

    @staticmethod
    def handler(user_id: int, data_source, title: str, description: str = None, **kwargs):
        return SnapshotsService().create_snapshot(
            user_id=user_id, operation_chain=data_source, title=title, description=description
        )

    @staticmethod
    def data_source(user_id: int, op_instance: "SaveSnapshotOperation"):
        # Need the current operation‐chain operations to save
        return ExplorerCacheService().extract_operation_chain_operations(user_id=user_id)

@register_operation
class DeleteSnapshotOperation(BaseOperation):
    name = "delete_snapshot"
    category = OperationCategory.SNAPSHOT
    result_type = OperationResultType.STATUS
    http_method = "DELETE"
    display = DisplayMode.NONE  # (could be an icon, but original config said "delete_button" under setup)

    @classmethod
    def arguments(cls) -> List[Argument]:
        return [
            Argument("user_id", int, required=True),
            Argument("snapshot_id", str, required=True),
        ]

    @staticmethod
    def handler(user_id: int, snapshot_id: str, **kwargs):
        return SnapshotsService().delete_snapshot(user_id=user_id, snapshot_id=snapshot_id)

@register_operation
class UpdateSnapshotOperation(BaseOperation):
    name = "update_snapshot"
    category = OperationCategory.SNAPSHOT
    result_type = OperationResultType.SNAPSHOT
    http_method = "PUT"
    display = DisplayMode.UPDATE_BUTTON

    @classmethod
    def arguments(cls) -> List[Argument]:
        return [
            Argument("user_id", int, required=True),
            Argument("snapshot_id", str, required=True),
            Argument("title", str, required=False),
            Argument("description", str, required=False),
        ]

    @staticmethod
    def handler(user_id: int, snapshot_id: str, title: str = None, description: str = None, **kwargs):
        return SnapshotsService().update_snapshot(
            user_id=user_id, snapshot_id=snapshot_id, title=title, description=description
        )

@register_operation
class GetSnapshotOperation(BaseOperation):
    name = "get_snapshot"
    category = OperationCategory.SNAPSHOT
    result_type = OperationResultType.SNAPSHOT
    http_method = "GET"
    display = DisplayMode.NONE

    @classmethod
    def arguments(cls) -> List[Argument]:
        return [
            Argument("user_id", int, required=True),
            Argument("snapshot_id", int, required=True),
        ]

    @staticmethod
    def handler(user_id: int, snapshot_id: int, **kwargs):
        return SnapshotsService().get_snapshot(user_id=user_id, snapshot_id=snapshot_id)

@register_operation
class GetAllSnapshotsOperation(BaseOperation):
    name = "get_all_snapshots"
    category = OperationCategory.SNAPSHOT
    result_type = OperationResultType.SNAPSHOT_LIST
    http_method = "GET"
    display = DisplayMode.LOAD_OPTIONS

    @classmethod
    def arguments(cls) -> List[Argument]:
        return [Argument("user_id", int, required=True)]

    @staticmethod
    def handler(user_id: int, **kwargs):
        return SnapshotsService().get_all_snapshots(user_id=user_id)
