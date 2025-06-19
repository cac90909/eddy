from typing import set

from shared.universal.enums import (
    UniversalColumn,
    DataType
)
from shared.universal.mappings import (
    UNIVERSAL_COLUMN_TO_DATATYPE,
    DATATYPE_TO_VALUE_PROVIDER
)
from shared.operation.mappings import (
    DATATYPE_TO_OPERATORS
)
from shared.operation.service import OperationService
from shared.operation.enums import (
    NON_FILTERABLE_COLUMNS
)
import shared.universal.util as UniversalUtil
# ----- Raw Universal -----

def get_column_name_options(user_id, data_src) -> set:
    """
    Retrieves list of valid column names that can have a filter operation applied:
    combining a list of Universal Columns + Universal Fields Column Keys 
    (which are drawn from the passed current data source)
    """
    #NOTE: expand scope to just column names instead of filter use case
    #      just naming and invoking all cols instead of non filt cols
    #      esp since non filt cols isnt a thing i think 
    #      (all ops dont use user maybe fields, thats it)
    all_cols = {col.value for col in UniversalColumn}
    filterable_cols = all_cols - NON_FILTERABLE_COLUMNS 
    filterable_keys = OperationService.get_unique_json_keys(user_id, data_src)
    return sorted(filterable_cols + filterable_keys)

def get_column_value_options(user_id, data_src, col_name):
    col_data_type = UniversalUtil.get_column_data_type(data_src, col_name)
    options_provider = DATATYPE_TO_VALUE_PROVIDER[col_data_type]
    return list(options_provider(user_id, data_src, col_name))

def get_filter_type_options(user_id, data_src, col_name):
    col_data_type = UniversalUtil.get_column_data_type(data_src, col_name)
    return DATATYPE_TO_OPERATORS[col_data_type]
    

def filter(self, user_id, data_src, col_name, filter_val, filter_type):
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