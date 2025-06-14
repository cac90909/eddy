# explorer/domain/operation.py
from dataclasses import dataclass
from typing import Mapping, Any

@dataclass(frozen=True)
class Operation:
    name: str
    arguments: Mapping[str, Any]
