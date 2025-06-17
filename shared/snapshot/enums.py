# core/snapshot/enums.py
from enum import Enum

class SnapshotColumns(str, Enum):
    SNAPSHOT_ID     = "snapshot_id"
    USER            = "user"
    TITLE           = "title"
    DESCRIPTION     = "description"
    OPERATION_CHAIN = "operation_chain"
    CREATED_AT      = "created_at"
    UPDATED_AT      = "updated_at"