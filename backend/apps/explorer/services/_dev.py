#Probably fold this into just static methods, not much purpose for class-ing this i think
from rest_framework.exceptions import APIException, NotFound
from typing import Any, List, Dict
from dataclasses import asdict

from explorer.services.operation_chain_manager import ExplorerOperationChainManager
from explorer.services.metadata_manager import ExplorerMetadataManager
from explorer.services.operation_executor import ExplorerOperationExecutorService
from core.services.operation_chain import OperationChainService
from core.domain.operation.structures.operation_chain import OperationChain

class ExplorerDevService:

    def __init__(self):
        self.cache_mgr = ExplorerOperationChainManager()
        self.meta_mgr = ExplorerMetadataManager()
        self.op_exec = ExplorerOperationExecutorService()
        self.core_op_chain_svc = OperationChainService()

    def get_operation_chain(self, user_id=2) -> List:
        try:
            op_chain = self.cache_mgr._get_chain(user_id)
            return op_chain.to_list()
        except Exception as e:
            raise APIException(e)

    def get_operation_chain_results(self, user_id=2) -> List:
        try:
            op_chain = self.cache_mgr._get_chain(user_id)
            return op_chain.strip_non_result_data()
        except Exception as e:
            raise APIException(e)

    def get_operation_chain_non_results(self, user_id=2) -> List[Dict]:
        try:
            op_chain = self.cache_mgr._get_chain(user_id)
            return op_chain.strip_result_data()
        except Exception as e:
            raise APIException(e)
        
    def get_current_metadata(self, user_id=2) -> Dict:
        return asdict(self.meta_mgr._get_metadata(user_id))
    
    def execute_op(self, op_name, op_args, user_id=2):
        data_src = self.cache_mgr.get_latest_result(user_id)
        op = self.core_op_chain_svc.handle_operation(user_id, op_name, data_src, **op_args)
        return op.result
