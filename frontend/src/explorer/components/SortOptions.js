// src/components/SortOptions.js
import React, { useState } from "react";
import { Box, TextField, MenuItem, Button } from "@mui/material";

function SortOptions({ columns, onSort }) {
  const [selectedColumn, setSelectedColumn] = useState(columns[0] || "");
  const [sortOrder, setSortOrder] = useState("asc");

  const handleSort = () => {
    console.log("SortOptions.handleSort called with:", { selectedColumn, sortOrder });
    if (!selectedColumn) {
      console.warn("No column selected for sorting");
      return;
    }
    onSort(selectedColumn, sortOrder);
  };

  return (
    <Box sx={{ display: "flex", flexDirection: "column", gap: 2 }}>
      <TextField
        select
        label="Sort Column"
        value={selectedColumn}
        onChange={(e) => setSelectedColumn(e.target.value)}
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
        label="Sort Order"
        value={sortOrder}
        onChange={(e) => setSortOrder(e.target.value)}
        variant="outlined"
      >
        <MenuItem value="asc">Ascending</MenuItem>
        <MenuItem value="desc">Descending</MenuItem>
      </TextField>
      <Button variant="contained" onClick={handleSort}>
        Apply Sort
      </Button>
    </Box>
  );
}

export default SortOptions;
