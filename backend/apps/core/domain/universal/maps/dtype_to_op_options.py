from core.domain.common.enums.data_types import DataType
from core.domain.universal.enums.operator_type import OperatorType

DATATYPE_TO_OPERATOR_OPTIONS = {
    DataType.INT:     [OperatorType.EQ.value, OperatorType.NEQ.value, OperatorType.LT.value, OperatorType.GT.value, OperatorType.LTE.value, OperatorType.GTE.value],
    DataType.FLOAT:   [OperatorType.EQ.value, OperatorType.NEQ.value, OperatorType.LT.value, OperatorType.GT.value, OperatorType.LTE.value, OperatorType.GTE.value],
    DataType.STRING:  [OperatorType.EQ.value, OperatorType.NEQ.value, OperatorType.STRING_CONTAINS.value, OperatorType.STRING_NOT_CONTAINS.value],
    DataType.DATE:    [OperatorType.EQ.value, OperatorType.NEQ.value, OperatorType.LT.value, OperatorType.GT.value, OperatorType.LTE.value, OperatorType.GTE.value],
    DataType.BOOLEAN: [OperatorType.EQ.value, OperatorType.NEQ.value],
    DataType.LIST:    [OperatorType.ARRAY_CONTAINS.value, OperatorType.ARRAY_NOT_CONTAINS.value],
}