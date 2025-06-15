# explorer/domain/operation_chain.py

from dataclasses import dataclass
from typing import List, Mapping, Any, Optional

from .operation import Operation

@dataclass
class OperationChain:
    operations: List[Operation]

    def append(self, op: Operation) -> None:
        self.operations.append(op)

    def pop(self) -> Optional[Operation]:
        return self.operations.pop() if self.operations else None

    def is_empty(self) -> bool:
        return not self.operations

    def to_list(self) -> List[Mapping[str, Any]]:
        return [op.__dict__ for op in self.operations]

    @classmethod
    def from_list(cls, raw: List[Mapping[str, Any]]) -> "OperationChain":
        return cls([Operation(**item) for item in raw])

    @property
    def last_operation(self) -> Optional[Operation]:
        """Return the most‐recently appended Operation, or None."""
        return self.operations[-1] if self.operations else None

    @property
    def last_result(self) -> Any:
        """
        Convenience to fetch the last command’s result_data field,
        or None if there is no last command.
        """
        return self.last_operation.result if self.last_operation else None
    
    def to_persistable(self) -> List[dict]:
        # used when saving a snapshot: drop the results
        return [op.non_result_data for op in self.operations]
    
    #NOTE: could use this for loading snapshot, loading these commands to re-create the operation
    #      chain, only having to repopulate the op chain with result data, but i dont think
    #      it entirely necessary since we have to re-run through all of the operations anyways.
    # @classmethod
    # def from_persisted(cls, raw: List[dict]) -> "OperationChain":
    #     # for snapshots: rebuild only the commands (results will be re-run)
    #     return cls([OperationEntry(name=d["operation_name"],
    #                                args=d["operation_arguments"],
    #                                result_type="unknown",
    #                                result_data=None)
    #                 for d in raw])
