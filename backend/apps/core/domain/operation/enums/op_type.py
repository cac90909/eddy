from enum import Enum

class OperationType(str, Enum):
    RAW      = "raw"
    LIST     = "list"
    METRIC   = "metric"
    ENRICHED = "enriched"