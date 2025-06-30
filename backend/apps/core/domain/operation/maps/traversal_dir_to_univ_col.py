from typing import Dict
from core.domain.operation.enums.traversal_directions import TraversalDirection
from core.domain.universal.enums.univ_columns import UniversalColumn


TRAVERSAL_DIRECTION_TO_UNIVERSAL_COLUMN: Dict[TraversalDirection, UniversalColumn] = {
    TraversalDirection.UPWARDS: UniversalColumn.PARENTS_IDS,
    TraversalDirection.DOWNWARDS: UniversalColumn.CHILDREN_IDS,
    TraversalDirection.HORIZONTAL: UniversalColumn.SIBLINGS_IDS
}