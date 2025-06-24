from enum import Enum

class StandardResponse(str, Enum):
    DATA = "data"
    META = "meta"

class HTTPMethod(str, Enum):
    GET = "get"
    POST = "post"
    PUT = "put"
    DELETE = "delete"