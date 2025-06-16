# explorer/enums/universal_col_name.py
from enum import Enum

class UniversalColumnName(str, Enum):
    DATE                = "date"
    FUNCTIONALITIES     = "functionalities"
    SUBJECT_MATTERS     = "subject_matters"
    GENERAL_CATEGORIES  = "general_categories"
    TITLE               = "title"
    TEXT                = "text"
    TAGS                = "tags"
    PARENTS_IDS         = "parents_ids"
    CHILDREN_IDS        = "children_ids"
    SIBLINGS_IDS        = "siblings_ids"
    FIELDS              = "fields"
    ENTRY_ID            = "entry_id"
    USER                = "user"
