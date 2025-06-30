from enum import Enum

class FrequencyType(str, Enum):
    DAILY   = "daily"
    WEEKLY  = "weekly"
    MONTHLY = "monthly"
    YEARLY  = "yearly"