from enum import Enum
from backend.apps.core.domain.universal.enums.univ_columns import UniversalColumn

TRAVERSABLE_COLUMNS = {
    UniversalColumn.PARENTS_IDS,
    UniversalColumn.CHILDREN_IDS,
    UniversalColumn.SIBLINGS_IDS
}