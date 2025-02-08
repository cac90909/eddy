// src/components/CategoricalFilter.js
import React, { useState, useEffect } from 'react';
import { Box, TextField, MenuItem, Button } from '@mui/material';

function CategoricalFilter({ columns, onApply, onGetUniqueFilterOptions }) {
  const [selectedColumn, setSelectedColumn] = useState('');
  const [options, setOptions] = useState([]);
  const [selectedValue, setSelectedValue] = useState('');

  // When the selected column changes, load unique options for that column.
  useEffect(() => {
    if (selectedColumn) {
      console.log("Fetching unique options for column:", selectedColumn);
      const fetchOptions = async () => {
        try {
          // Call the method passed in as a prop rather than importing explorerService directly.
          const response = await onGetUniqueFilterOptions(selectedColumn);
          console.log("Unique options for", selectedColumn, ":", response.data);
          setOptions(response.data);
          setSelectedValue(''); // Reset selected value when column changes
        } catch (error) {
          console.error("Error fetching options for column", selectedColumn, error);
          setOptions([]);
        }
      };
      fetchOptions();
    } else {
      setOptions([]);
      setSelectedValue('');
    }
  }, [selectedColumn, onGetUniqueFilterOptions]);

  const handleApplyFilter = () => {
    console.log("Apply filter clicked with:", { selectedColumn, selectedValue });
    if (selectedColumn && selectedValue) {
      onApply(selectedColumn, selectedValue);
    } else {
      console.warn("Both a column and a filter value must be selected.");
    }
  };

  return (
    <Box sx={{ display: 'flex', flexDirection: 'column', gap: 2 }}>
      <TextField
        select
        label="Select Column"
        value={selectedColumn}
        onChange={(e) => setSelectedColumn(e.target.value)}
        variant="outlined"
      >
        {columns.map((col) => (
          <MenuItem key={col} value={col}>
            {col}
          </MenuItem>
        ))}
      </TextField>

      {selectedColumn && (
        <TextField
          select
          label="Select Value"
          value={selectedValue}
          onChange={(e) => setSelectedValue(e.target.value)}
          variant="outlined"
        >
          {options.map((opt) => (
            <MenuItem key={opt} value={opt}>
              {opt}
            </MenuItem>
          ))}
        </TextField>
      )}

      <Button variant="contained" onClick={handleApplyFilter}>
        Apply Filter
      </Button>
    </Box>
  );
}

export default CategoricalFilter;
