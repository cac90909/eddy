from typing import NamedTuple
from core.domain.universal.enums.operator_type import OperatorType

class LookupItem(NamedTuple):
    lookup_suffix: str  
    exclude: bool 
OPERATOR_LOOKUPS: dict[str, LookupItem] = {
OperatorType.EQ.value:                LookupItem("", False),
OperatorType.NEQ.value:               LookupItem("", True),
OperatorType.LT.value:                LookupItem("__lt", False),
OperatorType.GT.value:                LookupItem("__gt", False),
OperatorType.LTE.value:               LookupItem("__lte", False),
OperatorType.GTE.value:               LookupItem("__gte", False),
OperatorType.STRING_CONTAINS.value:   LookupItem("__icontains", False),
OperatorType.ARRAY_CONTAINS.value:    LookupItem("__contains", False),
OperatorType.ARRAY_NOT_CONTAINS.value:LookupItem("__contains", True),
OperatorType.IS_NULL.value:           LookupItem("__isnull", False),
OperatorType.IS_NOT_NULL.value:       LookupItem("__isnull", False),
OperatorType.HAS_KEY.value:           LookupItem("__has_key", False),
OperatorType.DOESNT_HAVE_KEY.value:   LookupItem("__has_key", True),
OperatorType.IN.value:                LookupItem("__in", False),
OperatorType.NOT_IN.value:            LookupItem("__in", True)
}