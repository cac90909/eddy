// src/components/DateFilter.js
import React, { useState } from "react";
import { Box, TextField, MenuItem, Button } from "@mui/material";

function DateFilter({ onApply }) {
  const [date, setDate] = useState("");
  const [condition, setCondition] = useState("on"); // Default to "on"

  const handleApply = () => {
    console.log("DateFilter.apply called with:", { date, condition });
    if (!date) {
      console.warn("A date must be provided to apply the filter.");
      return;
    }
    // Pass the date and condition to the callback
    onApply(date, condition);
  };

  return (
    <Box sx={{ display: "flex", flexDirection: "column", gap: 2 }}>
      <TextField
        label="Select Date"
        type="date"
        value={date}
        onChange={(e) => setDate(e.target.value)}
        InputLabelProps={{ shrink: true }} // Ensures the label stays visible when using type="date"
        variant="outlined"
      />
      <TextField
        select
        label="Condition"
        value={condition}
        onChange={(e) => setCondition(e.target.value)}
        variant="outlined"
      >
        <MenuItem value="on">On</MenuItem>
        <MenuItem value="before">Before</MenuItem>
        <MenuItem value="after">After</MenuItem>
      </TextField>
      <Button variant="contained" onClick={handleApply}>
        Apply Date Filter
      </Button>
    </Box>
  );
}

export default DateFilter;
