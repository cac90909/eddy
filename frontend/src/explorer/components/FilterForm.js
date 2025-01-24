import React from "react";
import { TextField, Button, MenuItem } from "@mui/material";
import { useFilter } from "../hooks/useFilter";

function FilterForm({ onApplyFilter, columns }) {
  const {
    filterColumn,
    setFilterColumn,
    filterCondition,
    setFilterCondition,
    filterValue,
    setFilterValue,
    applyFilter,
  } = useFilter(onApplyFilter);

  return (
    <form style={{ display: "flex", gap: "1rem", alignItems: "center" }}>
      <TextField
        select
        label="Column"
        value={filterColumn}
        onChange={(e) => setFilterColumn(e.target.value)}
        variant="outlined"
      >
        {columns.map((col) => (
          <MenuItem key={col} value={col}>
            {col}
          </MenuItem>
        ))}
      </TextField>
      <TextField
        select
        label="Condition"
        value={filterCondition}
        onChange={(e) => setFilterCondition(e.target.value)}
        variant="outlined"
      >
        <MenuItem value="=">=</MenuItem>
        <MenuItem value=">">&gt;</MenuItem>
        <MenuItem value="<">&lt;</MenuItem>
      </TextField>
      <TextField
        label="Value"
        value={filterValue}
        onChange={(e) => setFilterValue(e.target.value)}
        variant="outlined"
      />
      <Button variant="contained" color="primary" onClick={applyFilter}>
        Apply Filter
      </Button>
    </form>
  );
}

export default FilterForm;
