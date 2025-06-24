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