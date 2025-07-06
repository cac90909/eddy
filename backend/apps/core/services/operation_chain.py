from typing import Any, Dict

from backend.apps.core.domain.operation.structures.operation import Operation
from backend.apps.core.domain.operation.structures.operation_chain import OperationChain
from core.domain.operation.enums.op_arg_name import OperationArgumentName
from core.services.operation import OperationService
from backend.apps.core.domain.operation.maps.op_name_to_spec import OPERATION_SPECS

class OperationChainService:

    def __init__(self):
         self.op_svc = OperationService()

    def assemble_from_chain(self, user_id: int, chain: OperationChain) -> OperationChain:
            """
            Replay each OperationCommand in the given chain in sequence,
            passing the prior result into the next operation.
            """
            op_list = []
            data_src = None
            for op in chain.operations:
                exec_op = self.handle_operation(
                    user_id=user_id,
                    op_name=op.name,
                    kwargs=op.args,
                    data_src=data_src
                )
                new_op = Operation(name=op.name, args=op.args, type=op.type, result=exec_op.result)
                op_list.append(new_op)
            op_chain = OperationChain(op_list)
            return op_chain

    
    def handle_operation(self, user_id, op_name, data_src=None, **kwargs) -> Operation:
        op_spec = OPERATION_SPECS[op_name]
        if data_src is not None:
             kwargs = {**kwargs, data_src:data_src}
        #NOTE: replace this later, making sure keys are column names to satisfy the args type check
        enum_args: dict[OperationArgumentName, Any] = {}
        for key_str, val in kwargs.items():
            enum_key = OperationArgumentName(key_str)
            enum_args[enum_key] = val
        res = op_spec.service_method(self.op_svc, user_id, **kwargs)
        op = Operation(name=op_name, 
                                args=enum_args, 
                                type=op_spec.result_type, 
                                result=res)
        return op