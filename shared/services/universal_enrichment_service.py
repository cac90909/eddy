from django.db.models import Count, Avg, Sum, Min, Max
from shared.repositories.universal_repository import UniversalRepository, build_orm_filter
from shared.logger import debug_print
from shared.util import catch_exceptions_cls



@catch_exceptions_cls(exception_return_value="Error", exclude=None)
class UniversalEnrichmentService:
    def __init__(self):
        self.universal_repository = UniversalRepository()

    # --- Grouped Aggregation Methods ---
    def group_aggregate_data(self, user_id, user_data, group_column, aggregate_operation, target_column, frequency=None):
        """
        Groups the given user_data by group_column (with optional date frequency conversion)
        and annotates each group with the specified aggregate operation on target_column.
        
        Parameters:
          - group_column: the column to group by (e.g., "date" or "gym_location").
          - frequency: (optional) if group_column is "date", one of "daily", "weekly", "monthly", or "yearly".
          - aggregate_operation: a string ("count", "average", "sum", "min", "max").
          - target_column: the column on which to perform the aggregate operation.
              NOTE: The client must supply the appropriate target column.
        
        Returns:
          A QuerySet of dictionaries with the effective group column and an aggregate value
          annotated as "result".
        """
        debug_print("Entering group_aggregate_data")
        # Mapping for aggregate operations using lambda functions.
        AGGREGATE_MAPPING = {
            "count": lambda target: Count(target) if target else Count("id"),
            "average": lambda target: Avg(target),
            "sum": lambda target: Sum(target),
            "min": lambda target: Min(target),
            "max": lambda target: Max(target),
        }
        #Creates an additional group columning based on the different possible values of initial group column ONLY if date is group column
        user_data, effective_group_column = self.universal_repository.apply_grouping(user_data, group_column, frequency)
        if aggregate_operation not in AGGREGATE_MAPPING:
            raise ValueError("Unsupported aggregate operation")
        aggregate_function = AGGREGATE_MAPPING[aggregate_operation](target_column)
        aggregate_dict = {"result": aggregate_function}
        grouped_data = self.universal_repository.group_by_data(user_data, effective_group_column, aggregate_dict)
        return grouped_data

    def filter_grouped_aggregate_data(self, user_id, user_data, group_column, aggregate_operation, target_column, operator, value, frequency=None):
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
        grouped = self.group_aggregate_data(user_id, user_data, group_column, aggregate_operation, target_column, frequency)
        filtered = self.universal_repository.filter_aggregates(grouped, operator, value, alias="result")
        return filtered
