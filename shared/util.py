import functools
import inspect

def _get_function_context(func, args):
    """
    Returns a tuple of (class_name, func_name, line_number).
    If the function is bound (i.e. its first argument is self), try to get the class name.
    """
    func_name = func.__name__
    line_number = func.__code__.co_firstlineno
    class_name = ""
    if args:
        instance = args[0]
        if hasattr(instance, '__class__'):
            class_name = instance.__class__.__name__
    return class_name, func_name, line_number

def log_vars_vals_func(func):
    """
    Decorator to log entry and exit for a function.
    Logs class name, function name, line number, arguments, and result.
    """
    @functools.wraps(func)
    def wrapper(*args, **kwargs):
        class_name, func_name, line_number = _get_function_context(func, args)
        print(f"[ENTER] {class_name}.{func_name} (line {line_number}) | args: {args}, kwargs: {kwargs}")
        result = func(*args, **kwargs)
        print(f"[EXIT] {class_name}.{func_name} (line {line_number}) | result: {result}")
        return result
    return wrapper

def catch_exceptions_func(default=None):
    """
    Decorator to catch exceptions in a function, log class name, function name,
    line number, arguments, and the exception, then return a default value.
    """
    def decorator(func):
        @functools.wraps(func)
        def wrapper(*args, **kwargs):
            class_name, func_name, line_number = _get_function_context(func, args)
            try:
                result = func(*args, **kwargs)
                return result
            except Exception as e:
                print(f"[EXCEPTION] {class_name}.{func_name} (line {line_number}) | Exception: {e} | args: {args}, kwargs: {kwargs}")
                return default
        return wrapper
    return decorator

def log_vars_vals_cls(exclude=None):
    """
    Class decorator that applies log_call to all callable methods in the class,
    except those whose names appear in the exclude list or special methods.
    """
    if exclude is None:
        exclude = []
    def decorator(cls):
        for attr_name, attr_value in cls.__dict__.items():
            # Skip special methods and those explicitly excluded.
            if attr_name.startswith("__") or attr_name in exclude:
                continue
            if callable(attr_value):
                decorated = log_vars_vals_func(attr_value)
                setattr(cls, attr_name, decorated)
        return cls
    return decorator

def catch_exceptions_cls(exception_return_value=None, exclude=None):
    """
    Class decorator that applies catch_exceptions(default=exception_return_value)
    to all callable methods in the class, except those whose names appear in the
    exclude list or special methods.
    """
    if exclude is None:
        exclude = []
    def decorator(cls):
        for attr_name, attr_value in cls.__dict__.items():
            if attr_name.startswith("__") or attr_name in exclude:
                continue
            if callable(attr_value):
                decorated = catch_exceptions_func(exception_return_value)(attr_value)
                setattr(cls, attr_name, decorated)
        return cls
    return decorator
