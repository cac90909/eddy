from dataclasses import dataclass
from typing import (
    Type, Any, Union,
    Callable, Dict, Tuple,
    Optional, Sequence, Iterable, Set
)
from core.domain.enums.operation import OperationArgumentName

@dataclass(frozen=True)
class ArgumentSpec:
    name: OperationArgumentName
    dtype: type
    required: bool
    multiple: Optional[bool] = False
    validate_fn: Optional[Callable[[Any, Dict[str, Any]], bool]] | Set = lambda value, ctx: True
    error_msg: Optional[str] = ""
    choices_fn: Optional[Callable[..., Sequence[Any]]] = None
    choices: Optional[Iterable[Any]] = None