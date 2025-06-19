import inspect
from typing import Any, Callable, Dict

def invoke_choices_fn(
    fn: Callable[..., Any],
    *,
    user_id: int,
    data_src: Any,
    args: Dict[str, Any],
) -> Any:
    """
    Call fn by matching its parameters to (user_id, data_src, args[...]).
    Raises if fn asks for something we didn’t supply.
    """
    sig   = inspect.signature(fn)
    bound = {}
    for name, param in sig.parameters.items():
        if name == "user_id":
            bound[name] = user_id
        elif name == "data_src":
            bound[name] = data_src
        elif name in args:
            bound[name] = args[name]
        else:
            raise ValueError(f"Missing argument {name!r} for {fn.__name__}")
    return fn(**bound)