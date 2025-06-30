from typing import Callable
from django.db.models.functions import TruncDay, TruncWeek, TruncMonth, TruncYear
from core.domain.universal.enums.frequency_type import FrequencyType

FREQUENCY_FUNCTIONS: dict[FrequencyType, Callable]= {
    FrequencyType.DAILY:   TruncDay,
    FrequencyType.WEEKLY:  TruncWeek,
    FrequencyType.MONTHLY: TruncMonth,
    FrequencyType.YEARLY:  TruncYear,
}