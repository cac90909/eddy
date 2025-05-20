from pydantic import BaseModel, Field, ValidationError
from typing import Any, Dict, List, Union, Optional


class InitUserParams(BaseModel):
    user_id: int

class FilterParams(BaseModel):
    column_name: str
    filter_type: str
    filter_value: Union[str, int, float]

class TraverseParams(BaseModel):
    start_id: int

class SortParams(BaseModel):
    column_names: List
    ascending: Optional