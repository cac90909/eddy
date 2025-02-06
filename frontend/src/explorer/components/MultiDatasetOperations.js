// src/components/MultiDatasetOperations.js
import React, { useState } from "react";
import { Box, TextField, MenuItem, Button, Typography } from "@mui/material";

function MultiDatasetOperations({ onOperate }) {
  const [operationType, setOperationType] = useState("");
  const [param, setParam] = useState("");

  const handleOperate = () => {
    console.log("MultiDatasetOperations.handleOperate called with:", { operationType, param });
    if (!operationType) {
      console.warn("Operation type must be selected");
      return;
    }
    onOperate(operationType, { param });
  };

  const operationOptions = [
    { value: "union", label: "Union" },
    { value: "join", label: "Join" }
    // Add more operations as needed.
  ];

  return (
    <Box sx={{ display: "flex", flexDirection: "column", gap: 2 }}>
      <Typography variant="h6">Multi-Dataset Operations</Typography>
      <TextField
        select
        label="Operation"
        value={operationType}
        onChange={(e) => setOperationType(e.target.value)}
        variant="outlined"
      >
        {operationOptions.map((option) => (
          <MenuItem key={option.value} value={option.value}>
            {option.label}
          </MenuItem>
        ))}
      </TextField>
      <TextField
        label="Parameter"
        value={param}
        onChange={(e) => setParam(e.target.value)}
        variant="outlined"
        placeholder="Enter additional parameter if needed"
      />
      <Button variant="contained" onClick={handleOperate}>
        Apply Operation
      </Button>
    </Box>
  );
}

export default MultiDatasetOperations;
