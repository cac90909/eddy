from enum import enum

class OperationType(str, Enum):
    RAW      = "raw"
    LIST     = "list"
    METRIC   = "metric"
    ENRICHED = "enriched"