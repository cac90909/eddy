// DataOverview.jsx
import React from "react";
import { Box, Typography } from "@mui/material";

const DataOverview = ({ dataType, dataAmount }) => {
  return (
    <Box sx={{ padding: "0.5rem" }}>
      <Typography variant="subtitle1" sx={{ fontWeight: "bold" }}>
        Data Overview
      </Typography>
      <Typography variant="body2">
        Data Type: {dataType || "N/A"}
      </Typography>
      <Typography variant="body2">
        Data Amount: {dataAmount !== undefined ? dataAmount : "N/A"}
      </Typography>
    </Box>
  );
};

export default DataOverview;
