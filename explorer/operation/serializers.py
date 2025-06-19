from rest_framework import serializers
from shared.universal.serializers import UniversalSerializer



# === Raw Operations ===
class GetFullDataRequestSerializer(serializers.Serializer):
    user_id = serializers.IntegerField()

class GetFullDataResponseSerializer(serializers.Serializer):
    data = UniversalSerializer(many=True)
    meta = serializers.DictField()

class FilterRequestSerializer(serializers.Serializer):
    user_id = serializers.IntegerField()
    column_name = serializers.CharField()
    filter_value = serializers.CharField()
    filter_type = serializers.CharField()

class FilterResponseSerializer(serializers.Serializer):
    data = UniversalSerializer(many=True)
    meta = serializers.DictField()

class TraverseRequestSerializer(serializers.Serializer):
    user_id = serializers.IntegerField()
    start_id = serializers.IntegerField()
    traversal_directions = serializers.ListField(
        child=serializers.ChoiceField(choices=["horizontal", "upwards", "downwards"]),
        allow_empty=False
    )
    

class TraverseResponseSerializer(serializers.Serializer):
    data = UniversalSerializer(many=True)
    meta = serializers.DictField()

# === List / Utility Operations ===
class GetUniqueColumnValuesRequestSerializer(serializers.Serializer):
    user_id = serializers.IntegerField()
    column_name = serializers.CharField()

class UniqueColumnValuesResponseSerializer(serializers.Serializer):
    """
    Returned by GET /operations/get_unique_column_values/:
    - data: a list of values (strings, ints, etc.) for the requested column
    """
    data = serializers.ListField(
        child=serializers.JSONField(),
        help_text="Unique values for the requested column"
    )
    meta = serializers.DictField(
        help_text="Additional metadata",
        default={}
    )

class GetUniqueJsonKeysRequestSerializer(serializers.Serializer):
    user_id = serializers.IntegerField()

class UniqueJsonKeysResponseSerializer(serializers.Serializer):
    """
    Returned by GET /operations/get_unique_json_keys/:
    - data: a list of all unique JSON keys present in the dataset
    """
    data = serializers.ListField(
        child=serializers.CharField(),
        help_text="Unique JSON keys found in the dataset"
    )
    meta = serializers.DictField(
        help_text="Additional metadata",
        default={}
    )

class GetUniqueJsonValuesRequestSerializer(serializers.Serializer):
    user_id = serializers.IntegerField()

class UniqueJsonValuesResponseSerializer(serializers.Serializer):
    """
    Returned by GET /operations/get_unique_json_values/:
    - data: a list of all unique JSON *values* across every key
    """
    data = serializers.ListField(
        child=serializers.JSONField(),
        help_text="Unique JSON values found in the dataset"
    )
    meta = serializers.DictField(
        help_text="Additional metadata",
        default={}
    )

class GetUniqueJsonKeyValuesRequestSerializer(serializers.Serializer):
    user_id = serializers.IntegerField()
    json_key = serializers.CharField()

class UniqueJsonKeyValuesResponseSerializer(serializers.Serializer):
    """
    Returned by GET /operations/get_unique_json_key_values/:
    - data: a list of unique values for a single JSON key
    """
    data = serializers.ListField(
        child=serializers.JSONField(),
        help_text="Unique values for the specified JSON key"
    )
    meta = serializers.DictField(
        help_text="Additional metadata",
        default={}
    )

# === Metric Operations ===
class GetCountRequestSerializer(serializers.Serializer):
    user_id = serializers.IntegerField()
    column_name = serializers.CharField()

class CountResponseSerializer(serializers.Serializer):
    """
    Response for get_count operation:
      - data: integer count of matching rows
    """
    data = serializers.IntegerField(
        help_text="Count of matching rows"
    )
    meta = serializers.DictField(
        help_text="Additional metadata",
        default={}
    )

class GetAverageRequestSerializer(serializers.Serializer):
    user_id = serializers.IntegerField()
    column_name = serializers.CharField()

class AverageResponseSerializer(serializers.Serializer):
    """
    Response for get_average operation:
      - data: floating-point average of the column
    """
    data = serializers.FloatField(
        help_text="Average value of the specified column"
    )
    meta = serializers.DictField(
        help_text="Additional metadata",
        default={}
    )

class GetSumRequestSerializer(serializers.Serializer):
    user_id = serializers.IntegerField()
    column_name = serializers.CharField()

class SumResponseSerializer(serializers.Serializer):
    """
    Response for get_sum operation:
      - data: integer sum of the column
    """
    data = serializers.IntegerField(
        help_text="Sum of all values in the specified column"
    )
    meta = serializers.DictField(
        help_text="Additional metadata",
        default={}
    )


class GetMinRequestSerializer(serializers.Serializer):
    user_id = serializers.IntegerField()
    column_name = serializers.CharField()

class MinResponseSerializer(serializers.Serializer):
    """
    Response for get_min operation:
      - data: the minimum value of the column (could be numeric or date)
    """
    data = serializers.JSONField(
        help_text="Minimum value (numeric or date string) of the specified column"
    )
    meta = serializers.DictField(
        help_text="Additional metadata",
        default={}
    )

class GetMaxRequestSerializer(serializers.Serializer):
    user_id = serializers.IntegerField()
    column_name = serializers.CharField()

class MaxResponseSerializer(serializers.Serializer):
    """
    Response for get_max operation:
      - data: the maximum value of the column (could be numeric or date)
    """
    data = serializers.JSONField(
        help_text="Maximum value (numeric or date string) of the specified column"
    )
    meta = serializers.DictField(
        help_text="Additional metadata",
        default={}
    )

# === Enriched Operations ===
class GroupAggregateRequestSerializer(serializers.Serializer):
    user_id = serializers.IntegerField()
    group_columns = serializers.ListField(child=serializers.CharField())
    aggregate_operation = serializers.CharField()
    target_column = serializers.CharField()
    frequency = serializers.ChoiceField(choices=["daily", "weekly", "monthly", "yearly"], required=False)

class GroupAggregateResponseSerializer(serializers.Serializer):
    """
    Response for group_aggregate operation:
      - data: a list of dicts, where each dict represents one group’s aggregated values
      - meta: any additional metadata (e.g. timing, warnings)
    """
    data = serializers.ListField(
        child=serializers.DictField(
            child=serializers.JSONField()
        ),
        help_text="List of grouped & aggregated result rows"
    )
    meta = serializers.DictField(
        help_text="Additional metadata",
        default={}
    )