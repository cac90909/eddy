import React, { useState, useEffect } from "react";

function DropdownFilter({ column, options = [], dynamicOptions = false, columns, data = [], onApplyFilter }) {
  const [selectedValue, setSelectedValue] = useState("");
  const [dynamicList, setDynamicList] = useState([]);

  // If dynamicOptions is true, dynamically generate dropdown options from the dataset
  useEffect(() => {
    if (dynamicOptions && columns.includes(column)) {
      const uniqueValues = new Set();

      // Generate unique values from the provided data
      data.forEach((row) => {
        if (Array.isArray(row[column])) {
          row[column].forEach((value) => uniqueValues.add(value));
        }
      });

      setDynamicList([...uniqueValues]);
    }
  }, [dynamicOptions, column, columns, data]);

  const handleSubmit = () => {
    if (!selectedValue) {
      alert("Please select a value.");
      return;
    }
    onApplyFilter({
      columnName: column,
      filterCondition: "contains", // Dropdown filters typically use "contains"
      filterValue: selectedValue,
    });
  };

  return (
    <div>
      <h4>{column.replace("_", " ")} Filter</h4>
      <select
        value={selectedValue}
        onChange={(e) => setSelectedValue(e.target.value)}
      >
        <option value="">Select a value</option>
        {(dynamicOptions ? dynamicList : options).map((option, idx) => (
          <option key={idx} value={option}>
            {option}
          </option>
        ))}
      </select>
      <button onClick={handleSubmit}>Apply</button>
    </div>
  );
}

export default DropdownFilter;
