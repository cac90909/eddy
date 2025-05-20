import React from 'react';
import { FormControl, InputLabel, Select, MenuItem } from '@mui/material';

export function OperationTypeSelector({ operationTypeOptions, selectedOperationType, onOperationTypeSelection }) {
  return (
    <FormControl fullWidth>
      <InputLabel>Operation Type</InputLabel>
      <Select value={selectedType || ""} label="Operation Type" onChange={onOperationTypeSelection}>
        {operationTypeOptions.map(selectedOperationType => (
          <MenuItem key={selectedOperationType} value={selectedOperationType}>
            {selectedOperationType.charAt(0).toUpperCase() + selectedOperationType.slice(1)}
          </MenuItem>
        ))}
      </Select>
    </FormControl>
  );
}
