import React from "react";
import { Box, Chip } from "@mui/material";

const ListDisplay = ({ data }) => {
  // Assume data is an array of values.
  return (
    <Box sx={{ padding: "1rem", display: "flex", flexWrap: "wrap", gap: 1 }}>
      {data.map((item, index) => (
        <Chip key={index} label={item} />
      ))}
    </Box>
  );
};

export default ListDisplay;
