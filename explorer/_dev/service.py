#Probably fold this into just static methods, not much purpose for class-ing this i think
from rest_framework.exceptions import APIException, NotFound
from typing import Any

import shared.operation.arguments.util as OperationArgumentsUtil
from shared.operation.specs import OPERATION_SPECS
from shared.operation.domain import OperationSpec, ArgumentSpec
from shared.operation.enums import OperationName, OperationArgumentName
from explorer.cache.service import ExplorerCacheService

class ExplorerDevService:

    def __init__(self):
        self.cache_svc = ExplorerCacheService()

    def get_operation_chain(self, user_id):
        try:
            op_chain = self.cache_svc.get_chain(user_id)
        except Exception as e:
            raise APIException(e)

    def get_operation_chain_results(self, user_id):
        try:
            op_chain = self.cache_svc.get_chain(user_id)
            results = [op.result for op in op_chain.operations]
            return results
        except Exception as e:
            raise APIException(e)

    def get_operation_chain_non_results(self, user_id):
        try:
            op_chain = self.cache_svc.get_chain(user_id)
            non_results = [op.non_result_data for op in op_chain.operations]
            return non_results
        except Exception as e:
            raise APIException(e)
