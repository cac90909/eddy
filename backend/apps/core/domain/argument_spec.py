
@dataclass(frozen=True)
class ArgumentSpec:
    name: OperationArgumentName
    required: bool
    multiple: Optional[bool] = False
    validate_fn: Optional[Callable[[Any, Dict[str, Any]], bool]] | Set = lambda value, ctx: True
    error_msg: Optional[str] = ""
    choices_fn: Optional[Callable[..., Sequence[Any]]] = None
    choices: Optional[Iterable[Any]] = None