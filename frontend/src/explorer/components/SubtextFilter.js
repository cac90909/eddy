// src/components/SubtextFilter.js
import React, { useState } from "react";
import { Box, TextField, MenuItem, Button } from "@mui/material";

function SubtextFilter({ columns, onApply }) {
  // Default selection: choose the first column in the provided list
  const [selectedColumn, setSelectedColumn] = useState(columns[0] || "");
  const [searchText, setSearchText] = useState("");

  const handleApply = () => {
    console.log("SubtextFilter.apply called with:", { selectedColumn, searchText });
    if (!selectedColumn || !searchText.trim()) {
      console.warn("Both a column and search text must be provided.");
      return;
    }
    onApply(selectedColumn, searchText);
  };

  return (
    <Box sx={{ display: "flex", flexDirection: "column", gap: 2 }}>
      <TextField
        select
        label="Select Column"
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
        label="Search Text"
        value={searchText}
        onChange={(e) => setSearchText(e.target.value)}
        variant="outlined"
      />
      <Button variant="contained" onClick={handleApply}>
        Apply Subtext Filter
      </Button>
    </Box>
  );
}

export default SubtextFilter;
