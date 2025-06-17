from shared.universal.repository import UniversalRepository
from shared.logger import debug_print, debug_print_vars
from shared.util import log_vars_vals_cls, catch_exceptions_cls
from shared.universal.repository import UniversalRepository
from shared.operation.mappings import CONTAINS_OPERATOR_MAP
import shared.universal.util as UniversalUtil 

#@log_vars_vals_cls(exclude=None)
@catch_exceptions_cls(exception_return_value="Error", exclude=None)
class OperationService:
    def __init__(self):
        self.uni_rep = UniversalRepository()

    # ----- Raw Universal -----

    def get_full_data(self, user_id):
        return self.uni_rep.get_full_data(user_id)

    def filter(self, user_id, data_src, col_name, filter_val, filter_type):
        if filter_type in CONTAINS_OPERATOR_MAP:
            col_data_type = UniversalUtil.get_column_data_type(data_src, col_name)
            filter_type = CONTAINS_OPERATOR_MAP.get(filter_type).get(col_data_type)
        return self.uni_rep.filter_data(data_src, col_name, filter_val, filter_type)

    def traverse(self, user_id, data_src, start_id, traversal_dirs):
        return self.uni_rep.traverse_data(data_src, start_id, traversal_dirs)
    
    # ----- Metric (Simple Aggregations) -----
    
    def get_count(self, user_id, data_src, col_name):   
        return self.uni_rep.get_count(data_src, col_name)

    def get_min(self, user_id, data_src, col_name):   
        return self.uni_rep.get_min(data_src, col_name)
    
    def get_max(self, user_id, data_src, col_name):   
        return self.uni_rep.get_max(data_src, col_name)
    
    def get_sum(self, user_id, data_src, col_name):   
        return self.uni_rep.get_sum(data_src, col_name)
    
    def get_average(self, user_id, data_src, col_name):   
        return self.uni_rep.get_average(data_src, col_name)
    
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

