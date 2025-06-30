from dataclasses import dataclass
from typing import Tuple, Callable, Type, Optional, Iterable, Sequence, Set, Any, Dict

from core.domain.operation.enums import (
    OperationName,
    OperationType,
    OperationArgumentName
)
from backend.apps.core.domain.operation.structures.argument_spec import ArgumentSpec

@dataclass(frozen=True)
class OperationSpec:
    name: OperationName
    result_type: OperationType
    args: Tuple[ArgumentSpec, ...]
    description: str
    service_method: Callable[..., Any]
