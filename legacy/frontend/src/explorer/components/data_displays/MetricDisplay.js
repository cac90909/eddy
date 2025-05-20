import React from "react";
import { Paper, Typography } from "@mui/material";

const MetricDisplay = ({ data }) => {
  // Assume data is a single value or a small object.
  return (
    <Paper sx={{ padding: "1rem", textAlign: "center" }}>
      <Typography variant="h3">{data}</Typography>
    </Paper>
  );
};

export default MetricDisplay;
