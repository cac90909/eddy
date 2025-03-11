import React from 'react';
import { FormControl, InputLabel, Select, MenuItem, TextoperationArgument, Grid } from '@mui/material';
import { useFetchValueOptions } from '../hooks/useFetchValueOptions';

// Helper: substitute placeholders in a string with values from formData.
// For simplicity, we assume placeholders start with '$' and match keys in formData.
const substitutePlaceholders = (template, formData) => {
  if (!template || typeof template !== 'string') return template;
  return template.replace(/\$([a-zA-Z_]+)/g, (_, key) => formData[key] || '');
};

export function OperationArgumentSelector({ operationArgument, operationArgumentsInput, onOperationArgumentSelection, userId }) {
  const operationArgumentKey = operationArgument.argument_name;
  
  // If the operationArgument has dynamic fetch instructions.
  if (operationArgument.value_options_fetch) {
    // Prepare operation arguments for fetching options.
    const dependencyPlaceholder = operationArgument.value_options_fetch_dependency; // e.g., "$column_name"
    const dependencyValue = dependencyPlaceholder ? substitutePlaceholders(dependencyPlaceholder, formData) : null;
    const dependencyOperationFetchArguments = dependencyValue 
      ? { user_id: userId, column_name: dependencyValue } 
      : { user_id: userId };

    const { valueOptions, loading, error } = useFetchValueOptions(operationArgument.value_options_fetch, 
        dependencyOperationFetchArguments);
    
    if (loading) return <Grid item xs={12}><p>Loading options for {operationArgumentKey}...</p></Grid>;
    if (error) return <Grid item xs={12}><p>Error loading options for {operationArgumentKey}</p></Grid>;
    
    return (
      <Grid item xs={12} sm={6}>
        <FormControl fullWidth>
          <InputLabel>{operationArgumentKey}</InputLabel>
          <Select
            value={operationArgumentsInput[operationArgumentKey] || ""}
            label={operationArgumentKey}
            onChange={(event) => onOperationArgumentSelection(operationArgumentKey, event.target.value)}
          >
            {valueOptions.map(option => (
              <MenuItem key={option} value={option}>
                {option}
              </MenuItem>
            ))}
          </Select>
        </FormControl>
      </Grid>
    );
  } else if (operationArgument.value_options) {
    // Static options provided directly.
    return (
      <Grid item xs={12} sm={6}>
        <FormControl fullWidth>
          <InputLabel>{operationArgumentKey}</InputLabel>
          <Select
            value={operationArgumentsInput[operationArgumentKey] || ""}
            label={operationArgumentKey}
            onChange={(e) => onOperationArgumentSelection(operationArgumentKey, e.target.value)}
          >
            {operationArgument.value_options.map(option => (
              <MenuItem key={option} value={option}>
                {option}
              </MenuItem>
            ))}
          </Select>
        </FormControl>
      </Grid>
    );
  } else {
    // No options: default to a text operationArgument.
    return (
      <Grid item xs={12} sm={6}>
        <TextoperationArgument
          fullWidth
          label={operationArgumentKey}
          value={operationArgumentsInput[operationArgumentKey] || ""}
          onChange={(event) => onOperationArgumentSelection(operationArgumentKey, event.target.value)}
        />
      </Grid>
    );
  }
}
