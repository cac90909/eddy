from django.db.models import Count, Avg, Sum, Min, Max
from core.domain.universal.enums.aggregation_type import AggregationType

AGGREGATION_FUNCTIONS = {
    AggregateType.COUNT: Count,
    AggregateType.AVG:   Avg,
    AggregateType.SUM:   Sum,
    AggregateType.MIN:   Min,
    AggregateType.MAX:   Max,
}