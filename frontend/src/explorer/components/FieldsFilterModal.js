// src/components/FieldsFilterModal.js
import React, { useState, useEffect } from "react";
import { Box, TextField, MenuItem, Button, Typography } from "@mui/material";

function FieldsFilterModal({ onApply }) {
  const [jsonKeys, setJsonKeys] = useState([]);
  const [selectedKey, setSelectedKey] = useState("");
  const [operationType, setOperationType] = useState("equals");
  const [value, setValue] = useState("");

  // Simulate loading JSON keys (could be fetched from a service)
  useEffect(() => {
    // Example static list of keys; replace with dynamic data as needed.
    setJsonKeys(["rating", "location", "duration"]);
  }, []);

  const handleApply = () => {
    console.log("FieldsFilterModal.handleApply called with:", { selectedKey, operationType, value });
    if (!selectedKey || !value) {
      console.warn("Both JSON key and value must be provided.");
      return;
    }
    onApply({ key: selectedKey, operation: operationType, value });
  };

  return (
    <Box sx={{ display: "flex", flexDirection: "column", gap: 2 }}>
      <Typography variant="h6">Fields Filter</Typography>
      <TextField
        select
        label="JSON Key"
        value={selectedKey}
        onChange={(e) => setSelectedKey(e.target.value)}
        variant="outlined"
      >
        {jsonKeys.map((key) => (
          <MenuItem key={key} value={key}>
            {key}
          </MenuItem>
        ))}
      </TextField>
      <TextField
        select
        label="Operation"
        value={operationType}
        onChange={(e) => setOperationType(e.target.value)}
        variant="outlined"
      >
        <MenuItem value="equals">Equals</MenuItem>
        <MenuItem value="contains">Contains</MenuItem>
        <MenuItem value="greater_than">Greater Than</MenuItem>
        <MenuItem value="less_than">Less Than</MenuItem>
      </TextField>
      <TextField
        label="Value"
        value={value}
        onChange={(e) => setValue(e.target.value)}
        variant="outlined"
      />
      <Button variant="contained" onClick={handleApply}>
        Apply Fields Filter
      </Button>
    </Box>
  );
}

export default FieldsFilterModal;
