from dataclasses import dataclass
from typing import Any, Callable, Dict, Tuple, Type, Optional, Mapping, List, Sequence, Set, Iterable
from core.domain.operation.enums import (
    OperationName,
    OperationType,
    OperationArgumentName
)

@dataclass(frozen=True)
class Operation:
    name: OperationName
    args: Mapping[OperationArgumentName, Any]
    type: OperationType
    result: Any

    @property
    def non_result_data(self) -> dict:
        """Strip out result data so we can persist only the instruction."""
        return {
            "name": self.name,
            "args": self.args,
            "type": self.type,
        }
    
    # @property
    # def spec(self):
    #     # lazy‐lookup the static metadata when you need it
    #     return get_spec_for_op(self.name)
    



