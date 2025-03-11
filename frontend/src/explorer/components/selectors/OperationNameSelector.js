import React from 'react';
import { FormControl, InputLabel, Select, MenuItem } from '@mui/material';

export function OperationNameSelector({ operationNameOptions, selectedOperationName, onOperationNameSelection }) {
  return (
    <FormControl fullWidth>
      <InputLabel>Operation</InputLabel>
      <Select value={selectedOperationName || ""} label="Operation" onChange={onOperationNameSelection}>
        {operationNameOptions.map(op => (
          <MenuItem key={op.operation_name} value={op.operation_name}>
            {op.operation_name.charAt(0).toUpperCase() + op.operation_name.slice(1)}
          </MenuItem>
        ))}
      </Select>
    </FormControl>
  );
}
