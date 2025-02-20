from django.db.models import Count, Avg, Sum, Min, Max
from shared.repositories.universal_repository import UniversalRepository, build_orm_filter
from shared.logger import debug_print
from shared.util import catch_exceptions_cls



@catch_exceptions_cls(exception_return_value="Error", exclude=None)
class UniversalMetricService:
    def __init__(self):
        self.universal_repository = UniversalRepository()

    def get_count(self, user_data, column_name):
        debug_print("Entering simple_count")
        return self.universal_repository.count_data(user_data, column_name)

    def get_average(self, user_data, column_name):
        debug_print("Entering simple_average")
        return self.universal_repository.average_data(user_data, column_name)

    def get_sum(self, user_data, column_name):
        debug_print("Entering simple_sum")
        return self.universal_repository.sum_data(user_data, column_name)

    def get_min(self, user_data, column_name):
        debug_print("Entering simple_min")
        return self.universal_repository.min_data(user_data, column_name)

    def get_max(self, user_data, column_name):
        debug_print("Entering simple_max")
        return self.universal_repository.max_data(user_data, column_name)

    
