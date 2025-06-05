from enum import Enum
from typing import Optional


class OperationCategory(Enum):
    """
    High-level buckets that describe what each Operation does.
    """
    DATA_QUERY = "data_query"         # anything that reads or filters the raw dataset
    TRANSFORM = "transform"           # joins, aggregations, traversals (i.e. reshapes data)
    METRIC = "metric"                 # count, avg, sum, min, max (returns a single number)
    UTILITY = "utility"               # fetch lists, get metadata, lookup values
    SESSION_STATE = "session_state"   # start/reset/undo/load/end session
    SNAPSHOT = "snapshot"             # save/delete/update/get snapshots


