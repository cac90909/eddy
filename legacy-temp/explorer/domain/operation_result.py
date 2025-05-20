import uuid
from datetime import datetime

class OperationResult():
    def __init__(self, data, meta):
        self.data = data
        self.meta = meta
        self.meta["created_at"] = datetime.now()
        self.meta["id"] = uuid.uuid4()

    def to_dict(self):
        return {
            "data": self.data,
            "meta": self.meta
        }

