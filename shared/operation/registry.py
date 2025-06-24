#NOTE: unsure if this class is needed


from .specs import OPERATION_SPECS
from .enums import OperationName
from shared.operation.domain import OperationSpec

def get_spec_for_op(op_name: str) -> OperationSpec:
    return OPERATION_SPECS[OperationName(op_name)]