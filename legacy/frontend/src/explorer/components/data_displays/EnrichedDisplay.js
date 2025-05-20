import React from "react";
import { Table, TableBody, TableCell, TableHead, TableRow, Paper } from "@mui/material";

const EnrichedDisplay = ({ data }) => {
  if (!data.length) return <p>No data available</p>;

  const columns = Object.keys(data[0]);
  const formatCellValue = (value) => {
    if (typeof value === "object" && value !== null) {
      return Array.isArray(value)
        ? value.join(", ")
        : Object.entries(value)
            .map(([key, val]) => `${key}: ${val}`)
            .join(", ");
    }
    return value !== null ? value : "";
  };

  // You can later add extra styling or dynamic behavior specific to enriched data.
  return (
    <Paper sx={{ overflowX: "auto", backgroundColor: "#f0f8ff" }}>
      <Table>
        <TableHead>
          <TableRow>
            {columns.map((col) => (
              <TableCell key={col} sx={{ fontWeight: "bold" }}>
                {col}
              </TableCell>
            ))}
          </TableRow>
        </TableHead>
        <TableBody>
          {data.map((row, idx) => (
            <TableRow key={idx}>
              {columns.map((col) => (
                <TableCell key={col}>{formatCellValue(row[col])}</TableCell>
              ))}
            </TableRow>
          ))}
        </TableBody>
      </Table>
    </Paper>
  );
};

export default EnrichedDisplay;
