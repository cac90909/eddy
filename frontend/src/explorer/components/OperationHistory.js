import React from "react";
import { Paper, Typography, List, ListItem, ListItemText } from "@mui/material";

const OperationHistory = ({ operationsHistory }) => {
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
        {operationsHistory && operationsHistory.length > 0 ? (
          operationsHistory.map((op, index) => (
            <ListItem key={index} dense>
              <ListItemText primary={op.operation_name} />
            </ListItem>
          ))
        ) : (
          <Typography variant="body2">No operations performed yet</Typography>
        )}
      </List>
    </Paper>
  );
};

export default OperationHistory;
