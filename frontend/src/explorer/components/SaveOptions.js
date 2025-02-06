// src/components/SaveOptions.js
import React, { useState } from "react";
import { Box, TextField, Button, Typography } from "@mui/material";

function SaveOptions({ onSave }) {
  const [alias, setAlias] = useState("");

  const handleTempSave = () => {
    console.log("SaveOptions.handleTempSave called with alias:", alias);
    if (!alias.trim()) {
      console.warn("Alias is required for temp save");
      return;
    }
    onSave("temp", alias);
  };

  const handleDeepSave = () => {
    console.log("SaveOptions.handleDeepSave called with alias:", alias);
    if (!alias.trim()) {
      console.warn("Alias is required for deep save");
      return;
    }
    onSave("deep", alias);
  };

  return (
    <Box sx={{ display: "flex", flexDirection: "column", gap: 2 }}>
      <Typography variant="h6">Save Options</Typography>
      <TextField
        label="Alias"
        value={alias}
        onChange={(e) => setAlias(e.target.value)}
        variant="outlined"
      />
      <Box sx={{ display: "flex", gap: 2 }}>
        <Button variant="contained" color="primary" onClick={handleTempSave}>
          Temp Save
        </Button>
        <Button variant="contained" color="secondary" onClick={handleDeepSave}>
          Deep Save
        </Button>
      </Box>
    </Box>
  );
}

export default SaveOptions;
