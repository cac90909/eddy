from django.db.models import Count, Avg, Sum, Min, Max
from shared.repositories.universal_repository import UniversalRepository, build_orm_filter
from shared.logger import debug_print
from shared.util import catch_exceptions_cls



@catch_exceptions_cls(exception_return_value="Error", exclude=None)
class UniversalEnrichmentService:
    def __init__(self):
        self.universal_repository = UniversalRepository()

    # --- Grouped Aggregation Methods ---
    def group_aggregate_data(self, user_id, data_source, group_column, aggregate_operation, target_column, frequency=None):
            """
            Delegates the group-aggregate operation to the repository.
            Expects data_source to be a QuerySet.
            
            Returns a tuple: (grouped_data, data_amount), where grouped_data is the grouped and annotated QuerySet
            and data_amount is the count of groups.
            """
            grouped_data = self.universal_repository.perform_group_aggregate(data_source, group_column, aggregate_operation, target_column, frequency)
            data_amount = grouped_data.count()
            return grouped_data, data_amount


    #NOTE: this filters post grouping agg (ex: group by artist, compute average of album ratings -> filter artists whose average rating is less than 5)
    #BUT, i would like to delegate this to my normal filter?? Honestly not sure at the moment. 
    def filter_grouped_aggregate_data(self, user_id, data_source, group_column, aggregate_operation, target_column, operator, value, frequency=None):
        """
        Groups the data and annotates with the specified aggregate, then applies a filter on the aggregated result.
        
        Parameters:
          - group_column: the column to group by.
          - frequency: (optional) if grouping by date.
          - aggregate_operation: a string ("count", "average", "sum", "min", "max").
          - target_column: the column on which to perform the aggregate.
          - operator: a high-level operator string (e.g., ">", "<=") for filtering.
          - value: the threshold value for filtering.
        
        Returns:
          A QuerySet of groups (as dictionaries) where the aggregated value (aliased as "result")
          satisfies the filter condition.
        """
        debug_print("Entering filter_grouped_aggregate_data")
        grouped_data = self.group_aggregate_data(user_id=user_id, data_source=data_source, group_column=group_column, 
                                                 aggregate_operation=aggregate_operation, target_column=target_column, frequency=frequency)
        filtered = self.universal_repository.filter_aggregates(grouped_qs=grouped_data, operator=operator, value=value, alias="result")
        data_amount = filtered.count()
        debug_print(data_amount)
        return filtered, data_amount
