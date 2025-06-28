from enum import Enum

class HTTP(str, Enum):
    GET = "get"
    POST = "post"
    PUT = "put"
    DELETE = "delete"