from shared.universal.repository import UniversalRepository
from shared.logger import debug_print, debug_print_vars
from shared.util import log_vars_vals_cls, catch_exceptions_cls
from shared.universal.repository import UniversalRepository
from shared.operation.mappings import (
    TRAVERSAL_DIRECTION_TO_UNIVERSAL_COLUMN
)
from shared.operation.enums import (
    TraversalDirection
)
from shared.universal.enums import (
    AggregateType,
    FrequencyType,
    UniversalColumn,
    OperatorType,
    TRAVERSABLE_COLUMNS
)
from django.db.models import QuerySet
from shared.models import Universal
from typing import Sequence, Any, Dict
import shared.operation.util as OpUtil
from shared.universal.util import bfs_traverse

#@log_vars_vals_cls(exclude=None)
@catch_exceptions_cls(exception_return_value="Error", exclude=None)
class OperationService:
    def __init__(self):
        self.univ_repo = UniversalRepository()

    # ----- Raw Universal -----

    def full_data(self, user_id):
        return self.univ_repo.get_full_data(user_id)

    def filter(self, 
               user_id, 
               data_source: QuerySet[Universal], 
               column: UniversalColumn, 
               value: Any, 
               operator: OperatorType):
        return self.univ_repo.filter_data(data_source, column, value, operator)

    def traverse(
            self, 
            user_id: int, 
            data_source: QuerySet[Universal], 
            start_id: Any, 
            traversal_dirs: Sequence[TraversalDirection],
    ) -> QuerySet[Universal]:
        """
        1) Translates traversal directions to model columns names 
        2) fetches all the node neighbors
        3) performs BFS on neighbors
        4) and then returns all neighbors that are reachable from the entry ID. 
        """
        graph_cols = [TRAVERSAL_DIRECTION_TO_UNIVERSAL_COLUMN[d]for d in traversal_dirs]
        neighbor_map = self.univ_repo.get_neighbors(data_source, graph_cols)
        visited_ids = bfs_traverse(neighbor_map, start_id)
        return self.univ_repo.get_rows_by_ids(data_source, visited_ids)
    
    # ----- Metric (Simple Aggregations) -----
    
    def simple_count(
            self, 
            user_id, 
            data_source: QuerySet[Universal], 
            column: UniversalColumn):   
        return self.univ_repo.aggregate_field(data_source, column, AggregateType.COUNT)

    def simple_min(
            self, 
            user_id, 
            data_source: QuerySet[Universal], 
            column: UniversalColumn):
        return self.univ_repo.aggregate_field(data_source, column, AggregateType.MIN)
    
    def simple_max(
            self, 
            user_id, 
            data_source: QuerySet[Universal], 
            column: UniversalColumn):
        return self.univ_repo.aggregate_field(data_source, column, AggregateType.MAX)
    
    def simple_sum(
            self, 
            user_id, 
            data_source: QuerySet[Universal], 
            column: UniversalColumn): 
        return self.univ_repo.aggregate_field(data_source, column, AggregateType.SUM)
    
    def simple_average(
            self, 
            user_id, 
            data_source: QuerySet[Universal], 
            column: UniversalColumn):
        return self.univ_repo.aggregate_field(data_source, column, AggregateType.AVG)
    
    # ----- List -----
    
    def unique_column_values(
            self, 
            user_id, 
            data_source: QuerySet[Universal], 
            column: UniversalColumn
        ):
        return self.univ_repo.get_unique_values(data_source, column)

    def unique_json_keys(
            self, 
            user_id, 
            data_source: QuerySet[Universal]
        ):
        return self.univ_repo.get_unique_json_keys(data_source)

    def unique_json_key_values(
            self, 
            user_id, 
            data_source: QuerySet[Universal], 
            key: str):
        return self.univ_repo.get_unique_json_key_values(data_source, key)
    
    def unique_json_values(
            self, 
            user_id, 
            data_source: QuerySet[Universal]
        ):
        return self.univ_repo.get_unique_json_values(data_source)
    
    # ----- Group Aggregations -----
    
    def group_count(
            self, 
            user_id, 
            data_source: QuerySet[Universal], 
            group_columns: list[UniversalColumn], 
            target_column: UniversalColumn, 
            frequency: FrequencyType|None = None
        ) -> QuerySet:
        aggregate_operation = AggregateType.COUNT
        return self.univ_repo.aggregate_group_fields(data_source, group_columns, aggregate_operation, target_column, frequency)
    
    def group_min(
            self, 
            user_id, 
            data_source: QuerySet[Universal], 
            group_columns: list[UniversalColumn], 
            target_column: UniversalColumn, 
            frequency: FrequencyType|None = None
        ) -> QuerySet:
        aggregate_operation = AggregateType.MIN
        return self.univ_repo.aggregate_group_fields(data_source, group_columns, aggregate_operation, target_column, frequency)
    
    def group_max(
            self, 
            user_id, 
            data_source: QuerySet[Universal], 
            group_columns: list[UniversalColumn], 
            target_column: UniversalColumn, 
            frequency: FrequencyType|None = None
        ) -> QuerySet:
        aggregate_operation = AggregateType.MAX
        return self.univ_repo.aggregate_group_fields(data_source, group_columns, aggregate_operation, target_column, frequency)
    
    def group_sum(
            self, 
            user_id, 
            data_source: QuerySet[Universal], 
            group_columns: list[UniversalColumn], 
            target_column: UniversalColumn, 
            frequency: FrequencyType|None = None
        ) -> QuerySet:
        aggregate_operation = AggregateType.SUM
        return self.univ_repo.aggregate_group_fields(data_source, group_columns, aggregate_operation, target_column, frequency)
    
    def group_average(
            self, 
            user_id, 
            data_source: QuerySet[Universal], 
            group_columns: list[UniversalColumn], 
            target_column: UniversalColumn, 
            frequency: FrequencyType|None = None
        ) -> QuerySet:
        aggregate_operation = AggregateType.AVG
        return self.univ_repo.aggregate_group_fields(data_source, group_columns, aggregate_operation, target_column, frequency)

