import React, { useState } from "react";

function TextFilter({ column, onApplyFilter }) {
  const [text, setText] = useState("");

  const handleSubmit = () => {
    if (!text.trim()) {
      alert("Please enter a value.");
      return;
    }
    onApplyFilter({
      columnName: column,
      filterCondition: "contains",
      filterValue: text,
    });
  };

  return (
    <div>
      <h4>{column.replace("_", " ")} Filter</h4>
      <input
        type="text"
        placeholder={`Search ${column}`}
        value={text}
        onChange={(e) => setText(e.target.value)}
      />
      <button onClick={handleSubmit}>Apply</button>
    </div>
  );
}

export default TextFilter;
