from core.domain.universal.enums.univ_columns import UniversalColumn
from core.domain.common.enums.data_types import DataType

UNIVERSAL_COLUMN_TO_DATATYPE: dict[UniversalColumn, DataType] = {
    UniversalColumn.DATE:              DataType.DATE,
    UniversalColumn.TEXT:              DataType.STRING,
    UniversalColumn.TITLE:             DataType.STRING,
    UniversalColumn.FUNCTIONALITIES:   DataType.LIST,
    UniversalColumn.SUBJECT_MATTERS:   DataType.LIST,
    UniversalColumn.GENERAL_CATEGORIES:DataType.LIST,
    UniversalColumn.TAGS:              DataType.LIST,
    UniversalColumn.PARENTS_IDS:       DataType.LIST,
    UniversalColumn.CHILDREN_IDS:      DataType.LIST,
    UniversalColumn.SIBLINGS_IDS:      DataType.LIST,
    UniversalColumn.ENTRY_ID:          DataType.STRING,
    UniversalColumn.FIELDS:            DataType.JSON,
    UniversalColumn.USER:              DataType.STRING,
}