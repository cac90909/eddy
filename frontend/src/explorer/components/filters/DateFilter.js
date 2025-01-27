import React, { useState } from "react";

function DateFilter({ column, onApplyFilter }) {
  const [date, setDate] = useState("");
  const [condition, setCondition] = useState("equals"); // "before", "after", or "equals"

  const handleSubmit = () => {
    if (!date) {
      alert("Please select a date.");
      return;
    }
    onApplyFilter({
      columnName: column,
      filterCondition: condition,
      filterValue: date,
    });
  };

  return (
    <div>
      <h4>Date Filter</h4>
      <select value={condition} onChange={(e) => setCondition(e.target.value)}>
        <option value="before">Before</option>
        <option value="after">After</option>
        <option value="equals">Equals</option>
      </select>
      <input
        type="date"
        value={date}
        onChange={(e) => setDate(e.target.value)}
      />
      <button onClick={handleSubmit}>Apply</button>
    </div>
  );
}

export default DateFilter;
