from enum import Enum
from typing import Optional

class OperationResultType(Enum):
    """
    What kind of payload the Operation returns.
    """
    RAW = "raw"                       # a raw dataset (e.g. a table or list of rows)
    ENRICHED = "enriched"             # an enriched dataset (post‐aggregation or transform)
    LIST = "list"                     # just a Python list of values (e.g. unique column values)
    METRIC = "metric"                 # a single scalar (int/float) result
    OPERATION_CHAIN = "operation_chain"
    OPERATIONS_LIST = "operations_list"
    RESULTS_LIST = "results_list"
    CONFIG = "config"                 # e.g. when starting a session you might return config data
    SNAPSHOT = "snapshot" 
    SNAPSHOT_LIST = "snapshot_list"             # a serialized snapshot object or a list thereof
    STATUS = "status"                 # a simple success/failure indicator (e.g. delete, end session)
