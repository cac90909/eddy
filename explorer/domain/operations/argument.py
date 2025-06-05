from typing import Any, Callable, Optional

class Argument:
    def __init__(
        self,
        name: str,
        type_: type,
        required: bool = False,
        multiple: bool = False,
        options_fetch: Optional[Callable[..., list[Any]]] = None,
        dependency: Optional[str] = None,
        default: Optional[Any] = None,
        help_text: Optional[str] = None,
    ):
        """
        :param name:            The key the frontend will send (e.g. "column_name").
        :param type_:           Python type (e.g. str, int, list, etc.).
        :param required:        True if missing this arg is an error.
        :param multiple:        True if frontend can submit a list of values.
        :param options_fetch:   If provided, a callable(user_id, **prev_args) → List[valid options].
        :param dependency:      If options_fetch depends on another argument (like "$column_name").
        :param default:         Default value if not supplied.
        :param help_text:       A brief description for UI/tooltips.
        """
        self.name = name
        self.type_ = type_
        self.required = required
        self.multiple = multiple
        self.options_fetch = options_fetch
        self.dependency = dependency
        self.default = default
        self.help_text = help_text

    def fetch_options(self, user_id: int, **provided_args) -> list[Any]:
        """
        Call this at runtime to populate dropdowns, if `options_fetch` is set.
        `provided_args` contains the values of any dependencies.
        """
        if not self.options_fetch:
            return []
        return self.options_fetch(user_id=user_id, **provided_args)