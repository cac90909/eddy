from shared.operation.mappings import (
    CONTAINS_OPERATOR_MAP
)
import shared.universal.util as UniversalUtil

def translate_filter_label(filter_type, col_name):
    if filter_type in CONTAINS_OPERATOR_MAP:
        UniversalUtil.get_column_primitive_type() 