from enum import Enum

class StandardResponse(str, Enum):
    DATA = "data"
    META = "meta"