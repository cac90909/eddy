from dataclasses import dataclass
from typing import Optional

@dataclass
class DataShape():
    num_rows: int = 0
    num_cols: int = 0
    num_vals: int = 0