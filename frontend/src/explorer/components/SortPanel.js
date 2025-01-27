import React, { useState } from "react";

function SortPanel({ onSort }) {
  const [sortOrder, setSortOrder] = useState("asc"); // Default to ascending

  const handleSort = () => {
    // Trigger the sort with the selected column and order
    onSort({
      columnName: "date",
      sortOrder, // "asc" or "desc"
    });
  };

  return (
    <div>
      <h4>Sort</h4>
      <select
        value={sortOrder}
        onChange={(e) => setSortOrder(e.target.value)}
      >
        <option value="asc">Ascending</option>
        <option value="desc">Descending</option>
      </select>
      <button onClick={handleSort}>Apply Sort</button>
    </div>
  );
}

export default SortPanel;
