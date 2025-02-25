from shared.repositories.universal_repository import UniversalRepository
from shared.logger import debug_print
from shared.util import catch_exceptions_cls
from shared.repositories.universal_repository import UniversalRepository




@catch_exceptions_cls(exception_return_value="Error", exclude=None)
class UniversalMetricService:
    def __init__(self):
        self.universal_repository = UniversalRepository()

    def get_count(self, data_source, column_name):   
        agg_res = self.universal_repository.get_aggregate(user_data_queryset=data_source, column_name=column_name, aggregate_type="count")
        return agg_res

    def get_average(self, data_source, column_name):
        agg_res = self.universal_repository.get_aggregate(user_data_queryset=data_source, column_name=column_name, aggregate_type="average")
        return agg_res

    def get_sum(self, data_source, column_name):
        agg_res = self.universal_repository.get_aggregate(user_data_queryset=data_source, column_name=column_name, aggregate_type="sum")
        return agg_res
    
    def get_min(self, data_source, column_name):
        agg_res = self.universal_repository.get_aggregate(user_data_queryset=data_source, column_name=column_name, aggregate_type="min")
        return agg_res
    
    def get_max(self, data_source, column_name):
        agg_res = self.universal_repository.get_aggregate(user_data_queryset=data_source, column_name=column_name, aggregate_type="max")
        return agg_res
    
