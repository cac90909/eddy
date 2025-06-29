# explorer/services/eoperation_service.py
from typing import Any, Dict, Callable

from rest_framework.exceptions import APIException, NotFound
from backend.apps.explorer.services.cache import ExplorerCacheService
from backend.apps.explorer.services.metadata_calculator import ExplorerMetadataService
from core.services.operation_chain import OperationChainService


class ExplorerOperationExecutorService:
    """
    Service layer that wraps core Universal* services,
    caches each operation and its result for undo/replay,
    and uniformly handles exceptions.
    """

    def __init__(self):
        self.cache_svc    = ExplorerCacheService()
        self.metadata_svc = ExplorerMetadataService()
        self.op_chain_svc = OperationChainService()

    def handle_operation(self, user_id, op_name, **kwargs):
        try:
            previous = self.cache_svc.last_result(user_id)
            op_with_res, res = self.op_chain_svc.handle_operation(user_id, op_name, previous, **kwargs)
            self.cache_svc.append_operation(user_id, op_with_res)
            res_meta = self.metadata_svc.generate_result_metadata(op_with_res.result, op_with_res.type)
            self.cache_svc.set_current_shape(user_id, res_meta)
            self.cache_svc.increment_operation_count(user_id)
            return res, res_meta
        except Exception as e:
            raise APIException(str(e))
