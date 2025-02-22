// OperationHistory.js
import React, { useEffect, useState } from "react";
import { Box, Typography, List, ListItem, ListItemText, Paper } from "@mui/material";
import ExplorerService from "../services/ExplorerService";

const OperationHistory = ({ userId, refreshKey }) => {
  const [operationChain, setOperationChain] = useState([]);

  useEffect(() => {
    if (!userId) return;
    const fetchChain = async () => {
      try {
        const response = await ExplorerService.getOperationChain(userId);
        // Assuming the response structure is { data: [ { operation_name, ... }, ... ] }
        setOperationChain(response.data || []);
      } catch (error) {
        console.error("Error fetching operation chain:", error);
      }
    };
    fetchChain();
  }, [userId, refreshKey]);

  return (
    <Paper 
      style={{
        height: "100%",
        overflowY: "auto",
        padding: "0.5rem",
        backgroundColor: "#e0e0e0",
        borderRadius: "4px",
      }}
    >
      <Typography variant="subtitle1" color="textSecondary">
        Operation History
      </Typography>
      <List>
        {operationChain.map((op, index) => (
          <ListItem key={index} dense>
            <ListItemText primary={op.operation_name} />
          </ListItem>
        ))}
      </List>
    </Paper>
  );
};

export default OperationHistory;
