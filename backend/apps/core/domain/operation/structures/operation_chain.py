from dataclasses import dataclass
from typing import List, Mapping, Any, Optional, Dict
from backend.apps.core.domain.operation.structures.operation import Operation

@dataclass
class OperationChain:
    operations: List[Operation]

    def append(self, op: Operation) -> None:
        self.operations.append(op)

    def pop(self) -> Optional[Operation]:
        if len(self.operations) == 0:
            raise Exception("Cannot pop chain of size 0")
        return self.operations.pop()

    def is_empty(self) -> bool:
        return not self.operations

    def to_list(self) -> List[Mapping[str, Any]]:
        return [op.__dict__ for op in self.operations]
    
    def strip_result_data(self) -> List[Dict]:
        return [op.non_result_data for op in self.operations]
    
    def strip_non_result_data(self) -> List:
        return [op.result for op in self.operations]

    @classmethod
    def from_list(cls, raw: List[Mapping[str, Any]]) -> "OperationChain":
        return cls([Operation(**item) for item in raw])

    @property
    def latest_operation(self) -> Operation:
        """Return the most‐recently appended Operation, or None."""
        if len(self.operations) == 0:
            raise Exception("Cannot retrieve latest operation on chain of size 0")
        return self.operations[-1] 

    @property
    def latest_result(self) -> Any:
        """
        Convenience to fetch the last command’s result_data field,
        or None if there is no last command.
        """
        if len(self.operations) == 0:
            raise Exception("Cannot retrieve latest operation result on chain of size 0")
        return self.operations[-1].result
    
    @property
    def chain_length(self) -> int:
        return len(self.operations)
    
    def to_persistable(self) -> List[dict]:
        # used when saving a snapshot: drop the results
        return [op.non_result_data for op in self.operations]
