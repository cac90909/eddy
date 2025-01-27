import React, { useState } from "react";

function SavePanel({ onSave }) {
  const [alias, setAlias] = useState("");

  const handleSave = () => {
    if (!alias.trim()) {
      console.error("Alias is required for saving");
      return;
    }
    onSave(alias);
  };

  return (
    <div>
      <h3>Save Options</h3>
      <input
        type="text"
        placeholder="Enter alias"
        value={alias}
        onChange={(e) => setAlias(e.target.value)}
      />
      <button onClick={handleSave}>Temp Save</button>
      <button onClick={handleSave}>Deep Save</button>
    </div>
  );
}

export default SavePanel;
