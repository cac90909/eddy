from dataclasses import dataclass
from typing import Any, Callable, Dict, Tuple, Type, Optional, Mapping, List, Sequence, Set, Iterable
from .enums import OperationName, OperationType, OperationArgumentName
from shared.operation.registry import get_spec_for_op

@dataclass(frozen=True)
class ArgumentSpec:
    name: OperationArgumentName
    required: bool
    multiple: Optional[bool] = False
    validate_fn: Optional[Callable[[Any, Dict[str, Any]], bool]] | Set = lambda value, ctx: True
    error_msg: Optional[str] = ""
    choices_fn: Optional[Callable[..., Sequence[Any]]] = None
    choices: Optional[Iterable[Any]] = None

@dataclass(frozen=True)
class OperationSpec:
    name: OperationName
    result_type: OperationType
    args: Tuple[ArgumentSpec, ...]
    description: str
    service_method: Callable[..., Any]
    response_serializer: Type[Any]

@dataclass(frozen=True)
class Operation:
    name: str
    args: Mapping[str, Any]
    type: str
    result: Any

    @property
    def non_result_data(self) -> dict:
        """Strip out result data so we can persist only the instruction."""
        return {
            "name": self.name,
            "args": self.args,
            "type": self.type,
        }
    
    @property
    def spec(self):
        # lazy‐lookup the static metadata when you need it
        return get_spec_for_op(self.name)

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
