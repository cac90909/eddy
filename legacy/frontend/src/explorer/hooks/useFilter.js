import { useState } from "react";

export const useFilter = (onApplyFilter) => {
  const [filterColumn, setFilterColumn] = useState("");
  const [filterCondition, setFilterCondition] = useState("=");
  const [filterValue, setFilterValue] = useState("");

  const applyFilter = () => {
    if (filterColumn && filterValue) {
      onApplyFilter({ columnName: filterColumn, filterCondition, filterValue });
    }
  };

  return {
    filterColumn,
    setFilterColumn,
    filterCondition,
    setFilterCondition,
    filterValue,
    setFilterValue,
    applyFilter,
  };
};
