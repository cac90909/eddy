# explorer/domain/operation.py
from dataclasses import dataclass
from typing import Mapping, Any

@dataclass(frozen=True)
class Operation:
    name: str
    args: Mapping[str, Any]
    type: str
    result: Any

    NAME   = "name"
    ARGS   = "args"
    TYPE   = "type"
    RESULT = "result"

    @property
    def non_result_data(self) -> dict:
        """Strip out result data so we can persist only the instruction."""
        return {
            self.NAME: self.name,
            self.ARGS: self.args,
            self.TYPE: self.type,
        }
