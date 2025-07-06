from dataclasses import dataclass
from typing import Optional

@dataclass
class BaseMetadata:
    session_start_time: str = "0"