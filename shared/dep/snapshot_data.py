import uuid
from datetime import datetime
from abc import ABC, abstractmethod
from shared.logger import debug_print

class SnapshotData(ABC):
    def __init__(self, data):
        self.data = data
        self.data_type = None
        self.created_at = datetime.now()
        self.id = uuid.uuid4()

# --- Universal Data Types ---

class Snapshot(SnapshotData):
    data_type = "snapshot"

class SnapshotList(SnapshotData):
    data_type = "snapshot_list"