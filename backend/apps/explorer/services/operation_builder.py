#Probably fold this into just static methods, not much purpose for class-ing this i think
from rest_framework.exceptions import APIException, NotFound
from typing import Any

import core.services.operation_arg_choices as OpArgsUtil
from backend.apps.core.domain.operation.maps.op_name_to_spec import OPERATION_SPECS
from backend.apps.core.domain.operation.structures.operation_spec import OperationSpec, ArgumentSpec
from explorer.services.operation_chain_manager import ExplorerOperationChainManager
from core.domain.operation.enums import OperationName
from core.domain.operation.enums import OperationArgumentName

class ExplorerLookupService:

    def __init__(self):
        self.chain_mgr = ExplorerOperationChainManager()

    def get_operation_argument_options(
        self,
        user_id: int,
        op_name: str,
        arg_name: str,
        prev_args: dict[str, Any],
    ) -> list[Any]:
        #NOTE: assumes all operations need prev data src right now
        try:
            matched_op_name = OperationName(op_name)
            op_spec: OperationSpec = OPERATION_SPECS[matched_op_name]
        except (ValueError, KeyError):
            raise NotFound(f"{op_name!r} is not a valid operation")
        try:
            matched_arg_name = OperationArgumentName(arg_name)
            arg_spec: ArgumentSpec = op_spec.args[matched_arg_name]
        except ValueError:
            raise NotFound(f"{arg_name!r} is not a valid argument of {op_name}")
        choices_fn = arg_spec.choices_fn
        if choices_fn is None:
            raise Exception(f"Missing Choices Function on specs: {op_spec.name, arg_spec.name}")
        data_src = self.chain_mgr.get_latest_result(user_id)
        try:
            options = OpArgsUtil.invoke_choices_fn(fn=choices_fn, user_id=user_id, data_src=data_src, args=prev_args)
            return options
        except Exception as e:
            raise APIException(e)
    
    def get_operation_info(self, operation_name):
        #info on arguments, description of arguments, description of operatoin
        pass