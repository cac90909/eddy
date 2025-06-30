from enum import Enum
from core.domain.universal.enums.univ_columns import UniversalColumn

NON_FILTERABLE_COLUMNS = {
    UniversalColumn.USER,
    UniversalColumn.CHILDREN_IDS,
    UniversalColumn.PARENTS_IDS,
    UniversalColumn.SIBLINGS_IDS,
    UniversalColumn.FIELDS
}