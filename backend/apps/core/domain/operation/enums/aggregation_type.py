from enum import Enum

class AggregationType(str, Enum):
    COUNT = "count"
    AVG   = "avg"
    SUM   = "sum"
    MIN   = "min"
    MAX   = "max"