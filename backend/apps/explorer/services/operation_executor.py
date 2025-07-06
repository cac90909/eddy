# explorer/services/eoperation_service.py
from typing import Any, Dict, Callable

from rest_framework.exceptions import APIException, NotFound
from backend.apps.explorer.services.metadata_manager import ExplorerMetadataManager
from core.services.operation_chain import OperationChainService
from explorer.services.operation_chain_manager import ExplorerOperationChainManager


class ExplorerOperationExecutorService:
    """
    Service layer that wraps core Universal* services,
    caches each operation and its result for undo/replay,
    and uniformly handles exceptions.
    """

    def __init__(self):
        self.metadata_mgr = ExplorerMetadataManager()
        self.chain_mgr = ExplorerOperationChainManager()
        self.chain_svc = OperationChainService()

    def handle_operation(self, user_id: int, op_name: str, **kwargs):
        try:
            previous = self.chain_mgr.get_latest_result(user_id)
            op = self.chain_svc.handle_operation(user_id, op_name, previous, **kwargs)
            self.chain_mgr.append_operation(user_id, op)
            op_meta = self.metadata_mgr.update_metadata(user_id, op)
            return op.result, op_meta
        except Exception as e:
            raise APIException(str(e))
