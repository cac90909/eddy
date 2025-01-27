import logging
from logging.handlers import RotatingFileHandler
import os

import inspect


def get_logger(name: str, log_file: str = "app.log", level=logging.INFO):
    """
    Create and configure a logger.
    :param name: Name of the logger (usually __name__).
    :param log_file: File to write logs to.
    :param level: Logging level (default: logging.INFO).
    :return: Configured logger.
    """
    logger = logging.getLogger(name)
    logger.setLevel(level)
    logger.info(f"Log file path: {os.path.abspath(log_file)}")

    # File handler
    file_handler = RotatingFileHandler(log_file, maxBytes=1_000_000, backupCount=3)
    file_handler.setFormatter(logging.Formatter("%(asctime)s - %(levelname)s - %(message)s"))

    # Stream handler
    stream_handler = logging.StreamHandler()
    stream_handler.setFormatter(logging.Formatter("%(levelname)s: %(message)s"))

    # Always add handlers (if they are not already added to this logger)
    if all(not isinstance(h, RotatingFileHandler) for h in logger.handlers):
        logger.addHandler(file_handler)
    if all(not isinstance(h, logging.StreamHandler) for h in logger.handlers):
        logger.addHandler(stream_handler)

    return logger

def debug_print(*args):
    """
    Prints a debug message with file name, line number, and function name.
    Accepts multiple arguments for the message.
    """
    frame = inspect.currentframe().f_back  # Get the caller's frame
    file_name = frame.f_code.co_filename   # Get the file name
    line_number = frame.f_lineno           # Get the line number
    function_name = frame.f_code.co_name   # Get the function name

    # Combine all arguments into a single message
    message = "||-----||".join(str(arg) for arg in args)
    print(f"[{file_name}:{line_number} - {function_name}] {message}")
    print()

def debug_request(request):
    print(f"Method: {request.method}")
    print(f"Path: {request.path}")
    print(f"Query Parameters: {request.GET}")
    print(f"Headers: {dict(request.headers)}")
    print(f"Body: {request.body.decode('utf-8') if request.body else 'No Body'}")
    print()

