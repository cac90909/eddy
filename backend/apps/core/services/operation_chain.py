from typing import Any, Dict

from backend.apps.core.domain.operation.structures.operation import Operation
from backend.apps.core.domain.operation.structures.operation_chain import OperationChain
from core.domain.operation.enums.op_arg_name import OperationArgumentName
from core.services.operation import OperationService
from backend.apps.core.domain.operation.maps.op_name_to_spec import OPERATION_SPECS

class OperationChainService:

    def __init__(self):
         self.op_svc = OperationService()

    def assemble_from_chain(self, user_id: int, chain: OperationChain) -> tuple[Any,Any]:
            """
            Replay each OperationCommand in the given chain in sequence,
            passing the prior result into the next operation.
            """
            ops_with_res, latest_res = [], None
            data_src = None
            for op in chain.operations:
                latest_res = self.handle_operation(
                    user_id=user_id,
                    op_name=op.name,
                    kwargs=op.args,
                    data_src=data_src
                )
                entry = Operation(name=op.name, args=op.args, type=op.type, result=latest_res)
                ops_with_res.append(entry)
            op_chain_with_res = OperationChain(ops_with_res)
            return op_chain_with_res, latest_res

    
    def handle_operation(self, user_id, op_name, data_src=None, **kwargs):
        op_spec = OPERATION_SPECS[op_name]
        if data_src is not None:
             kwargs = {**kwargs, data_src:data_src}
        #NOTE: replace this later, making sure keys are column names to satisfy the args type check
        enum_args: dict[OperationArgumentName, Any] = {}
        for key_str, val in kwargs.items():
            enum_key = OperationArgumentName(key_str)
            enum_args[enum_key] = val
        res = op_spec.service_method(self.op_svc, user_id, **kwargs)
        op_with_res = Operation(name=op_name, 
                                args=enum_args, 
                                type=op_spec.result_type, 
                                result=res)
        return op_with_res, res