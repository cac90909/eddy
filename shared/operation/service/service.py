from shared.universal.repository import UniversalRepository
from shared.logger import debug_print, debug_print_vars
from shared.util import log_vars_vals_cls, catch_exceptions_cls
from shared.universal.repository import UniversalRepository
from shared.operation.mappings import (
    CONTAINS_OPERATOR_MAP,
    TRAVERSAL_DIRECTION_TO_UNIVERSAL_COLUMN
)
from shared.universal.enums import (
    AggregationType
)
from django.db.models import QuerySet
from shared.models import Universal
import shared.universal.util as UniversalUtil 
from typing import Sequence, Any, Dict
import shared.operation.service.util as OpSvcUtil

#@log_vars_vals_cls(exclude=None)
@catch_exceptions_cls(exception_return_value="Error", exclude=None)
class OperationService:
    def __init__(self):
        self.univ_repo = UniversalRepository()

    # ----- Raw Universal -----

    def get_full_data(self, user_id):
        return self.univ_repo.get_full_data(user_id)

    def filter(self, user_id, data_src, col_name, filter_val, filter_type):
        return self.univ_repo.filter_data(data_src, col_name, filter_val, filter_type)

    def traverse(
            self, 
            user_id: int, 
            data_src: QuerySet[Universal], 
            start_id: Any, 
            traversal_dirs: Sequence[str],
    ) -> QuerySet[Universal]:
        """
        1) Translates traversal directions to model columns names 
        2) fetches all the node neighbors
        3) performs BFS on neighbors
        4) and then returns all neighbors that are reachable from the entry ID. 
        """
        graph_cols = [TRAVERSAL_DIRECTION_TO_UNIVERSAL_COLUMN[d]for d in traversal_dirs]
        neighbor_map = self.univ_repo.get_neighbors(data_src, graph_cols)
        visited_ids = OpSvcUtil.bfs_traverse(neighbor_map, start_id)
        return self.univ_repo.get_rows_by_ids(data_src, visited_ids)
    
    # ----- Metric (Simple Aggregations) -----
    
    def get_count(self, user_id, data_src, col_name):   
        return self.univ_repo.aggregate_field(data_src, col_name, AggregationType.COUNT)

    def get_min(self, user_id, data_src, col_name):   
        return self.univ_repo.aggregate_field(data_src, col_name, AggregationType.MIN)
    
    def get_max(self, user_id, data_src, col_name):   
        return self.univ_repo.aggregate_field(data_src, col_name, AggregationType.MAX)
    
    def get_sum(self, user_id, data_src, col_name):   
        return self.univ_repo.aggregate_field(data_src, col_name, AggregationType.SUM)
    
    def get_average(self, user_id, data_src, col_name):   
        return self.univ_repo.aggregate_field(data_src, col_name, AggregationType.AVG)
    
    # ----- List -----
    
    def get_unique_column_values(self, user_id, data_src, col_name):
        return self.uni_rep.get_unique_column_values(data_src, col_name)

    def get_unique_json_keys(self, user_id, data_src):
        return self.uni_rep.get_unique_json_keys(data_src)

    def get_unique_json_key_values(self, user_id, data_src, json_key):
        return self.uni_rep.get_unique_json_key_values(data_src, json_key)
    
    def get_unique_json_values(self, user_id, data_src):
        return self.uni_rep.get_unique_json_values(data_src)
    
    # ----- Group Aggregations -----
    
    def get_count_group_aggregate(self, user_id, data_src, grp_cols, tgt_col, freq=None):
        return self.uni_rep.get_count_group_aggregate(data_src, grp_cols, tgt_col, freq)
    
    def get_min_group_aggregate(self, user_id, data_src, grp_cols, tgt_col, freq=None):
        return self.uni_rep.get_min_group_aggregate(data_src, grp_cols, tgt_col, freq)
    
    def get_max_group_aggregate(self, user_id, data_src, grp_cols, tgt_col, freq=None):
        return self.uni_rep.get_max_group_aggregate(data_src, grp_cols, tgt_col, freq)
    
    def get_sum_group_aggregate(self, user_id, data_src, grp_cols, tgt_col, freq=None):
        return self.uni_rep.get_sum_group_aggregate(data_src, grp_cols, tgt_col, freq)
    
    def get_average_group_aggregate(self, user_id, data_src, grp_cols, tgt_col, freq=None):
        return self.uni_rep.get_average_group_aggregate(data_src, grp_cols, tgt_col, freq)

