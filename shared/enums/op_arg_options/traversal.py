# explorer/enums/traversal.py
from enum import Enum

class TraversalDirection(str, Enum):
    UPWARDS    = "upwards"
    DOWNWARDS  = "downwards"
    HORIZONTAL = "horizontal"