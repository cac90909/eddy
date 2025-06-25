from dataclasses import dataclass
from typing import Tuple, Callable, Type, Optional, Iterable, Sequence, Set, Any, Dict

from core.domain.enums.operation import OperationName, OperationType, OperationArgumentName


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
