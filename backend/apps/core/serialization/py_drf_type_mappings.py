from typing import Type, Any, Dict
from datetime import date, datetime, time
from rest_framework import serializers

PY_TO_DRF_FIELD: Dict[Type[Any], Type[serializers.Field]] = {
    str: serializers.CharField,
    int: serializers.IntegerField,
    float: serializers.FloatField,
    bool: serializers.BooleanField,
    date: serializers.DateField,
    datetime: serializers.DateTimeField,
    time: serializers.TimeField,
    dict: serializers.JSONField,       # for JSON blobs
    list: serializers.ListField,       # if you ever need a bare ListField
}