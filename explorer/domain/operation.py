# explorer/domain/operation.py
from dataclasses import dataclass
from typing import Mapping, Any

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
