import uuid
from datetime import datetime
from abc import ABC, abstractmethod
from shared.logger import debug_print
class StatusData:
    def __init__(self, status, message=None):
        self.status = status
        self.message = message
        self.data_type = "status"
        self.created_at = datetime.now()
        self.id = uuid.uuid4()

    def to_dict(self):
        return {
            'status': self.status,
            'message': self.message,
            'created_at': self.created_at,
            'id': self.id
        }

    @staticmethod
    def from_dict(data: dict):
        return StatusData(data['status'], data['message'])