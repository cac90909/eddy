import uuid
from datetime import datetime
from abc import ABC, abstractmethod
from shared.logger import debug_print

class UniversalData():
    def __init__(self, data):
        self.data = data
        self.data_type = None
        self.created_at = datetime.now()
        self.id = uuid.uuid4()
        self.data_overview = self.generate_data_overview()
    @abstractmethod
    def generate_data_overview(self):
        pass
    @abstractmethod
    def serialize(self):
        pass

# --- Universal Data Types ---

class UniversalRaw(UniversalData):
    data_type = "raw"
    def to_list(self):
        return list(self.data)
    def generate_data_overview(self):
        self.data_overview["num_rows"] = self.data.count()
        self.data_overview["num_columns"] = len(self.get_columns())
        self.data_overview["num_values"] = self.data_overview["num_rows"] * self.data_overview["num_columns"]
    d

class UniversalEnriched(UniversalData):
    data_type = "enriched"
    def to_list(self):
        return list(self.data)
    def generate_data_overview(self):
        self.data_overview["num_rows"] = self.data.count()
        self.data_overview["num_columns"] = len(self.get_columns())
        self.data_overview["num_values"] = self.data_overview["num_rows"] * self.data_overview["num_columns"]

class UniversalList(UniversalData):
    data_type = "list"
    def generate_data_overview(self):
        self.data_overview["num_values"] = len(self.data)

class UniversalMetric(UniversalData):
    data_type = "metric"
    def generate_data_overview(self):
        self.data_overview["num_values"] = 1