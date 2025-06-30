from enum import Enum
from typing import Type
from django.db.models import (
    CharField, TextField, BooleanField, IntegerField, FloatField, DateField, JSONField
)
from django.db.models import Field

DJANGO_FIELD_TYPES: set[Type[Field]] = {
    CharField,
    TextField,
    BooleanField,
    IntegerField,
    FloatField,
    DateField,
    JSONField
}