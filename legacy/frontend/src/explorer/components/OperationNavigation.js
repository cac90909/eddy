import React, { useState, useEffect } from 'react';
import { Box, Button, Grid, Typography } from '@mui/material';
import { OperationTypeSelector } from '.selectors/OperationTypeSelector';
import { OperationNameSelector } from '.selectors/OperationNameSelector';
import { OperationArgumentSelector } from '.selectors/OperationArgumentSelector';

export function OperationNavigation({ userId, operationConfigs, handleApplyOperation }) {
  // operationsList: operations that have display = "operation_navigation"
  const operationsList = operationConfigs.filter(op => op.display === "operation_navigation");

  // Derive unique operation types from operationsList.
  const uniqueTypes = Array.from(new Set(operationsList.map(op => op.operation_type)));

  const [selectedType, setSelectedType] = useState(""); 
  const [selectedOperationName, setSelectedOperationName] = useState("");
  const [currentOperation, setCurrentOperation] = useState(null);
  const [operationArgumentsInput, setOperationArgumentsInput] = useState({});

  // Set default type if not selected.
  useEffect(() => {
    if (uniqueTypes.length > 0 && !selectedType) {
      setSelectedType(uniqueTypes[0]);
    }
  }, [uniqueTypes, selectedType]);

  // Update currentOperation when selected type or operation name changes.
  useEffect(() => {
    if (selectedType) {
      const filteredOps = operationsList.filter(op => op.operation_type === selectedType);
      if (selectedOperationName) {
        const op = filteredOps.find(op => op.operation_name === selectedOperationName);
        setCurrentOperation(op);
        // Reset formData when a new operation is selected.
        setOperationArgumentsInput({});
      } else if (filteredOps.length > 0) {
        setSelectedOperationName(filteredOps[0].operation_name);
      }
    }
  }, [selectedType, selectedOperationName, operationsList]);

  const handleOperationArgumentSelection = (key, value) => {
    setOperationArgumentsInput(prev => ({ ...prev, [key]: value }));
  };

  const handleApplyOperation = async () => {
    if (!currentOperation) return;
    try {
      // The third parameter indicates updateGlobal is false.
      await handleApplyOperation(operationName=currentOperation.operation_name, 
        operationArgumentsInput=operationArgumentsInput, 
        updateGlobal=true);
    } catch (error) {
      console.error("Error applying operation:", error);
    }
  };

  return (
    <Box sx={{ p: 2, border: "1px solid #ccc", borderRadius: 2, mb: 2 }}>
      <Typography variant="h5" gutterBottom>
        Operation Navigation
      </Typography>
      <Grid container spacing={2}>
        {/* Operation Type Selector */}
        <Grid item xs={12} sm={4}>
          <OperationTypeSelector 
            operationTypeOptions={uniqueTypes} 
            selectedOperationType={selectedType} 
            onOperationTypeSelection={(event) => {
              setSelectedType(event.target.value);
              setSelectedOperationName("");
              setOperationArgumentsInput({});
            }}
          />
        </Grid>
        {/* Operation Name Selector */}
        <Grid item xs={12} sm={4}>
          <OperationNameSelector 
            operationNameOptions={operationsList.filter(op => op.operation_type === selectedType)}
            selectedOperation={selectedOperationName}
            onOperationNameSelection={(event) => setSelectedOperationName(event.target.value)}
          />
        </Grid>
        {/* Render dynamic form fields for current operation (skip user_id) */}
        {currentOperation && currentOperation.operation_arguments && 
          currentOperation.operation_arguments
            .filter(opArg => opArg.argument_name !== "user_id")
            .map(opArg => (
              <OperationArgumentSelector 
                key={arg.argument_name}
                operationArgument={opArg}
                operationArgumentsInput={operationArgumentsInput}
                onOperationArgumentSelection={handleOperationArgumentSelection}
                userId={userId}
              />
            ))
        }
        <Grid item xs={12}>
          <Button variant="contained" color="primary" onClick={handleApplyOperation} disabled={!currentOperation}>
            Apply
          </Button>
        </Grid>
      </Grid>
    </Box>
  );
}

export default OperationNavigation;
