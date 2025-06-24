# explorer/services/eoperation_service.py
from typing import Any, Dict, Callable

from rest_framework.exceptions import APIException, NotFound
from backend.apps.explorer.services.cache import ExplorerCacheService
from explorer.metadata.service import ExplorerMetadataService
from explorer.domain.operation import Operation
from core.operation.specs import OPERATION_SPECS
from core.operation.service import OperationService
from core.operation.enums import OperationArgumentName, OperationName


class OperationBuilderService:
    """
    Service layer that wraps core Universal* services,
    caches each operation and its result for undo/replay,
    and uniformly handles exceptions.
    """

    def __init__(self):
        self.op_svc = OperationService()
        self.cache_svc    = ExplorerCacheService()
        self.metadata_svc = ExplorerMetadataService()

    def handle_operation(self, user_id, op_name, **kwargs):
        try:
            previous = self.cache_svc.last_result(user_id)
            if previous is not None:
                kwargs = {**kwargs, OperationArgumentName.DATA_SOURCE.value:previous}
            op_spec = OPERATION_SPECS[op_name]
            res = op_spec.service_method(self.op_svc, user_id, **kwargs)
            entry = Operation(name=op_name, args=kwargs, type=op_spec.result_type, result=res)
            self.cache_svc.append_operation(user_id, entry)
            shape = self.metadata_svc.compute_data_shape(res, op_spec.result_type)
            self.cache_svc.set_current_shape(user_id, shape)
            self.cache_svc.increment_operation_count(user_id)
            return res, shape
        except Exception as e:
            raise APIException(str(e))
