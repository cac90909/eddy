from typing import Type
from core.domain.common.enums.data_types import DataType
from django.db.models import (
    CharField, 
    TextField, 
    BooleanField, 
    IntegerField, 
    FloatField, 
    DateField, 
    JSONField, 
    Field
)
from django.contrib.postgres.fields import ArrayField

FIELD_TO_DATA_TYPE = {
    CharField:    DataType.STRING,
    TextField:    DataType.STRING,
    BooleanField: DataType.BOOLEAN,
    IntegerField: DataType.INT,
    FloatField:   DataType.FLOAT,
    DateField:    DataType.DATE,
    ArrayField:   DataType.LIST,
    JSONField:    DataType.JSON,
}