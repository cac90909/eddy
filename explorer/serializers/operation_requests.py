from rest_framework import serializers



# === Raw Operations ===
class GetFullDataRequestSerializer(serializers.Serializer):
    user_id = serializers.IntegerField()

class FilterRequestSerializer(serializers.Serializer):
    user_id = serializers.IntegerField()
    column_name = serializers.CharField()
    filter_value = serializers.CharField()
    filter_type = serializers.CharField()

class TraverseRequestSerializer(serializers.Serializer):
    user_id = serializers.IntegerField()
    start_id = serializers.IntegerField()
    traversal_directions = serializers.ListField(child=serializers.ChoiceField(choices=["horizontal", "upwards", "downwards"]))

# === List / Utility Operations ===
class GetUniqueColumnValuesRequestSerializer(serializers.Serializer):
    user_id = serializers.IntegerField()
    column_name = serializers.CharField()

class GetUniqueJsonKeysRequestSerializer(serializers.Serializer):
    user_id = serializers.IntegerField()

class GetUniqueJsonValuesRequestSerializer(serializers.Serializer):
    user_id = serializers.IntegerField()

class GetUniqueJsonKeyValuesRequestSerializer(serializers.Serializer):
    user_id = serializers.IntegerField()
    json_key = serializers.CharField()

# === Metric Operations ===
class GetCountRequestSerializer(serializers.Serializer):
    user_id = serializers.IntegerField()
    column_name = serializers.CharField()

class GetAverageRequestSerializer(serializers.Serializer):
    user_id = serializers.IntegerField()
    column_name = serializers.CharField()

class GetSumRequestSerializer(serializers.Serializer):
    user_id = serializers.IntegerField()
    column_name = serializers.CharField()

class GetMinRequestSerializer(serializers.Serializer):
    user_id = serializers.IntegerField()
    column_name = serializers.CharField()

class GetMaxRequestSerializer(serializers.Serializer):
    user_id = serializers.IntegerField()
    column_name = serializers.CharField()

# === Enriched Operations ===
class GroupAggregateRequestSerializer(serializers.Serializer):
    user_id = serializers.IntegerField()
    group_columns = serializers.ListField(child=serializers.CharField())
    aggregate_operation = serializers.CharField()
    target_column = serializers.CharField()
    frequency = serializers.ChoiceField(choices=["daily", "weekly", "monthly", "yearly"], required=False)