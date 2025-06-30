from core.infrastructure.repositories.universal import UniversalRepository
from backend.apps.core.infrastructure.logging.logger import debug_print, debug_print_vars
from core.domain.operation.maps.traversal_dir_to_univ_col import TRAVERSAL_DIRECTION_TO_UNIVERSAL_COLUMN
from core.domain.operation.enums.traversal_directions import TraversalDirection
from core.domain.universal.enums.aggregation_type import AggregationType
from core.domain.universal.enums.frequency_type import FrequencyType
from core.domain.universal.enums.univ_columns import UniversalColumn
from core.domain.universal.enums.operator_type import OperatorType
from core.domain.universal.enums.traversable_cols import TRAVERSABLE_COLUMNS
from django.db.models import QuerySet
from core.models import Universal
from typing import Sequence, Any, Dict
from backend.apps.core.infrastructure.repositories.universal_util import bfs_traverse

#@log_vars_vals_cls(exclude=None)
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
        return self.univ_repo.aggregate_field(data_source, column, AggregationType.COUNT)

    def simple_min(
            self, 
            user_id, 
            data_source: QuerySet[Universal], 
            column: UniversalColumn):
        return self.univ_repo.aggregate_field(data_source, column, AggregationType.MIN)
    
    def simple_max(
            self, 
            user_id, 
            data_source: QuerySet[Universal], 
            column: UniversalColumn):
        return self.univ_repo.aggregate_field(data_source, column, AggregationType.MAX)
    
    def simple_sum(
            self, 
            user_id, 
            data_source: QuerySet[Universal], 
            column: UniversalColumn): 
        return self.univ_repo.aggregate_field(data_source, column, AggregationType.SUM)
    
    def simple_average(
            self, 
            user_id, 
            data_source: QuerySet[Universal], 
            column: UniversalColumn):
        return self.univ_repo.aggregate_field(data_source, column, AggregationType.AVG)
    
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
        aggregate_operation = AggregationType.COUNT
        return self.univ_repo.aggregate_group_fields(data_source, group_columns, aggregate_operation, target_column, frequency)
    
    def group_min(
            self, 
            user_id, 
            data_source: QuerySet[Universal], 
            group_columns: list[UniversalColumn], 
            target_column: UniversalColumn, 
            frequency: FrequencyType|None = None
        ) -> QuerySet:
        aggregate_operation = AggregationType.MIN
        return self.univ_repo.aggregate_group_fields(data_source, group_columns, aggregate_operation, target_column, frequency)
    
    def group_max(
            self, 
            user_id, 
            data_source: QuerySet[Universal], 
            group_columns: list[UniversalColumn], 
            target_column: UniversalColumn, 
            frequency: FrequencyType|None = None
        ) -> QuerySet:
        aggregate_operation = AggregationType.MAX
        return self.univ_repo.aggregate_group_fields(data_source, group_columns, aggregate_operation, target_column, frequency)
    
    def group_sum(
            self, 
            user_id, 
            data_source: QuerySet[Universal], 
            group_columns: list[UniversalColumn], 
            target_column: UniversalColumn, 
            frequency: FrequencyType|None = None
        ) -> QuerySet:
        aggregate_operation = AggregationType.SUM
        return self.univ_repo.aggregate_group_fields(data_source, group_columns, aggregate_operation, target_column, frequency)
    
    def group_average(
            self, 
            user_id, 
            data_source: QuerySet[Universal], 
            group_columns: list[UniversalColumn], 
            target_column: UniversalColumn, 
            frequency: FrequencyType|None = None
        ) -> QuerySet:
        aggregate_operation = AggregationType.AVG
        return self.univ_repo.aggregate_group_fields(data_source, group_columns, aggregate_operation, target_column, frequency)

