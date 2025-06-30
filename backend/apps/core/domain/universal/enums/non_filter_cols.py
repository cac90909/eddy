from enum import Enum
from core.domain.universal.enum.col_names import UniversalColumn

NON_FILTERABLE_COLUMNS = {
    UniversalColumn.USER.value,
    UniversalColumn.CHILDREN_IDS.value,
    UniversalColumn.PARENTS_IDS.value,
    UniversalColumn.SIBLINGS_IDS.value,
    UniversalColumn.FIELDS.value
}