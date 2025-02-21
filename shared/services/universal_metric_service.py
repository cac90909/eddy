from django.db.models import Count, Avg, Sum, Min, Max
from shared.repositories.universal_repository import UniversalRepository, build_orm_filter
from shared.logger import debug_print
from shared.util import catch_exceptions_cls



@catch_exceptions_cls(exception_return_value="Error", exclude=None)
class UniversalMetricService:
    def __init__(self):
        self.universal_repository = UniversalRepository()

    def get_count(self, data_source, column_name):
        
        count_data = self.universal_repository.count_data(data_source, column_name)
        debug_print(f"Count: {count_data}")
        return count_data

    def get_average(self, data_source, column_name):
        average_data = self.universal_repository.average_data(data_source, column_name)
        debug_print(f"Average: {average_data}")
        return average_data

    def get_sum(self, data_source, column_name):
        sum_data = self.universal_repository.sum_data(data_source, column_name)
        debug_print(f"Sum: {sum_data}")
        return sum_data
    
    def get_min(self, data_source, column_name):
        min_data = self.universal_repository.min_data(data_source, column_name)
        debug_print(f"Min: {min_data}")
        return min_data
    
    def get_max(self, data_source, column_name):
        max_data = self.universal_repository.max_data(data_source, column_name)
        debug_print(f"Max: {max_data}")
        return max_data
    
