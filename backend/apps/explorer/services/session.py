from core.services.snapshot import SnapshotsService
from core.services.operation import OperationService
from backend.apps.explorer.services.metadata_manager import ExplorerMetadataManager
from explorer.services.operation_executor import ExplorerOperationExecutorService
from explorer.services.operation_chain_manager import ExplorerOperationChainManager
from core.services.operation_chain import OperationChainService
from core.domain.operation.enums.op_name import OperationName
from core.domain.operation.structures.operation import Operation
from explorer.domain.structures.metadata import OperationMetadata
from rest_framework.exceptions import APIException
from typing import Any, Callable, Dict, Tuple

class ExplorerSessionService:

    def __init__(self):
        self.snap_svc = SnapshotsService()
        self.op_svc = OperationService()
        self.meta_mgr = ExplorerMetadataManager()
        self.op_exec = ExplorerOperationExecutorService()
        self.chain_svc = OperationChainService()
        self.chain_mgr = ExplorerOperationChainManager()

    def start_session(self, user_id) -> Tuple[Any, OperationMetadata]:
        try:
            op = self.chain_svc.handle_operation(user_id, OperationName.FULL_DATA.value)
            metadata = self.meta_mgr.initialize_metadata(user_id, op)
            self.chain_mgr.append_operation(user_id, op)
            return op.result, metadata
        except Exception as e:
            raise APIException(e)
        
    def reset_session(self, user_id) -> Tuple[Any, OperationMetadata]:
        try:
            self.chain_mgr.reset_chain(user_id)
            self.meta_mgr.reset_metadata(user_id)
            op_res, metadata = self.start_session(user_id)
            return op_res, metadata
        except Exception as e:
            raise APIException(e)
    
    def end_session(self, user_id) -> None:
        try:
            self.chain_mgr.delete_chain(user_id)
            self.meta_mgr.delete_metadata(user_id)
        except Exception as e:
            raise APIException(e)
    
    def undo_operation(self, user_id) -> Tuple[Any, OperationMetadata]:
        try:
            if self.chain_mgr._get_chain(user_id).chain_length < 2:
                raise Exception(f"Cannot undo on chain of length: {self.chain_mgr._get_chain(user_id).chain_length}")
            self.chain_mgr.pop_operation(user_id)
            latest_op = self.chain_mgr.get_latest_operation(user_id)
            meta = self.meta_mgr.update_metadata(user_id, latest_op)
            return latest_op.result, meta
        except Exception as e:
            raise APIException(e)
        
    def load_snapshot(self, user_id, snapshot_id) -> Tuple[Any, OperationMetadata]:
        try:
            self.chain_mgr.reset_chain(user_id)
            self.meta_mgr.reset_metadata(user_id)
            snapshot = self.snap_svc.get_snapshot(user_id, snapshot_id)
            chain = snapshot.operation_chain
            exec_chain = self.chain_svc.assemble_from_chain(user_id, chain)
            meta = self.meta_mgr.initialize_metadata(user_id, exec_chain.latest_operation)
            self.chain_mgr._set_chain(user_id, exec_chain)
            return exec_chain.latest_result, meta
        except Exception as e:
            raise APIException(e)
        








  