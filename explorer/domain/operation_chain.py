# explorer/domain/operation_chain.py

from dataclasses import dataclass
from typing import List, Mapping, Any, Optional

from .operation import Operation

@dataclass
class OperationChain:
    commands: List[Operation]

    def append(self, cmd: Operation) -> None:
        self.commands.append(cmd)

    def is_empty(self) -> bool:
        return not self.commands

    def to_list(self) -> List[Mapping[str, Any]]:
        return [cmd.__dict__ for cmd in self.commands]

    @classmethod
    def from_list(cls, raw: List[Mapping[str, Any]]) -> "OperationChain":
        return cls([Operation(**item) for item in raw])

    @property
    def last_command(self) -> Optional[Operation]:
        """Return the most‐recently appended Operation, or None."""
        return self.commands[-1] if self.commands else None

    @property
    def last_result(self) -> Any:
        """
        Convenience to fetch the last command’s result_data field,
        or None if there is no last command.
        """
        last = self.last_command
        return getattr(last, "result_data", None) if last else None
