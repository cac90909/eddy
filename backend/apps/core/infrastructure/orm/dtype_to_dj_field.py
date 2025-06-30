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

DATA_TYPE_TO_FIELD: dict[DataType, Type[Field]] = {
    DataType.STRING:  CharField,    
    DataType.BOOLEAN: BooleanField,
    DataType.INT:     IntegerField,
    DataType.FLOAT:   FloatField,
    DataType.DATE:    DateField,
    DataType.LIST:    ArrayField,
    DataType.JSON:    JSONField,
}