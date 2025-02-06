// src/components/TraversalOptions.js
import React from "react";
import { Box, Button, ButtonGroup, Typography } from "@mui/material";

function TraversalOptions({ onTraverse }) {
  // Define the available traversal directions.
  const directions = [
    "downward",
    "upward",
    "horizontal",
    "top_to_bottom",
    "top_to_bottom_horizontal"
  ];

  const handleClick = (direction) => {
    console.log("TraversalOptions: selected direction:", direction);
    if (onTraverse) {
      onTraverse(direction);
    }
  };

  return (
    <Box sx={{ display: "flex", flexDirection: "column", gap: 1 }}>
      <Typography variant="h6">Traversal Options</Typography>
      <ButtonGroup variant="contained" color="primary">
        {directions.map((direction, index) => (
          <Button key={index} onClick={() => handleClick(direction)}>
            {direction.replace(/_/g, " ").replace(/\b\w/g, (char) => char.toUpperCase())}
          </Button>
        ))}
      </ButtonGroup>
    </Box>
  );
}

export default TraversalOptions;
