from django.db.models import Count, Avg, Sum, Min, Max
from core.domain.universal.enums.aggregation_type import AggregationType

AGGREGATION_FUNCTIONS = {
    AggregationType.COUNT: Count,
    AggregationType.AVG:   Avg,
    AggregationType.SUM:   Sum,
    AggregationType.MIN:   Min,
    AggregationType.MAX:   Max,
}