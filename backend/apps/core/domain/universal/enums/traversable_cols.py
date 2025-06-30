from enum import Enum
from core.domain.universal.enums.univ_col_names import UniversalColumn

TRAVERSABLE_COLUMNS = {
    UniversalColumn.PARENTS_IDS,
    UniversalColumn.CHILDREN_IDS,
    UniversalColumn.SIBLINGS_IDS
}