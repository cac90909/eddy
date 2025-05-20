// DataOverview.jsx
import React from "react";
import { Box, Typography } from "@mui/material";

const DataOverview = ({ dataType, dataOverview }) => {
  const { current_data_shape = {}, full_data_shape = {} } = dataOverview || {};
  const { num_rows: cur_num_rows, num_columns: cur_num_columns, num_values: cur_num_values } = current_data_shape;
  const { num_rows: full_num_rows, num_columns: full_num_columns, num_values: full_num_values } = full_data_shape;


  return (
    <Box sx={{ padding: "0.5rem" }}>
      <Typography variant="subtitle1" sx={{ fontWeight: "bold" }}>
        Data Overview
      </Typography>
      <Typography variant="body2">
        Current Data Type: {dataType || "N/A"}
      </Typography>
      <Typography variant="body2">
        Current Data Shape: {cur_num_rows || "N/A"} rows, {cur_num_columns || "N/A"} columns, {cur_num_values || "N/A"} values
      </Typography>
      <Typography variant="body2">
        Full Data Shape: {full_num_rows || "N/A"} rows, {full_num_columns || "N/A"} columns, {full_num_values || "N/A"} values 
      </Typography>
    </Box>
  );
};

export default DataOverview;
