import React from "react";
import { Table, TableBody, TableCell, TableHead, TableRow } from "@mui/material";

function DataTable({ data }) {
  if (!data.length) return <p>No data available</p>;

  const columns = Object.keys(data[0]);

  // Helper function to format cell values
  const formatCellValue = (value) => {
    if (typeof value === "object" && value !== null) {
      // Format objects/arrays into a readable string
      return Array.isArray(value)
        ? value.join(", ") // Convert arrays to comma-separated strings
        : Object.entries(value)
            .map(([key, val]) => `${key}: ${val}`) // Convert objects to "key: value" strings
            .join(", ");
    }
    return value !== null ? value : ""; // Render other values as-is, handle null gracefully
  };

  return (
    <Table>
      <TableHead>
        <TableRow>
          {columns.map((col) => (
            <TableCell key={col} style={{ fontWeight: "bold" }}>
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
  );
}

export default DataTable;
