import React, { useState } from "react";

function ActionBar({ onPrevious, onUnion, onTempSave, onDeepSave, savedAliases }) {
  const [selectedAlias, setSelectedAlias] = useState(""); // For Union
  const [deepSaveAlias, setDeepSaveAlias] = useState(""); // For Deep Save

  const handleUnion = () => {
    if (!selectedAlias.trim()) {
      alert("Please select a dataset alias to union.");
      return;
    }
    onUnion(selectedAlias);
  };

  const handleDeepSave = () => {
    if (!deepSaveAlias.trim()) {
      alert("Please provide an alias for deep save.");
      return;
    }
    onDeepSave(deepSaveAlias);
  };

  return (
    <div>
      <h3>Actions</h3>
      <button onClick={onPrevious}>Previous</button> {/* Undo Button */}
      <button onClick={onTempSave}>Temp Save</button> {/* Temporary Save */}

      <div>
        <h4>Union</h4>
        <select
          value={selectedAlias}
          onChange={(e) => setSelectedAlias(e.target.value)}
        >
          <option value="">Select a dataset alias</option>
          {savedAliases.map((alias, idx) => (
            <option key={idx} value={alias}>
              {alias}
            </option>
          ))}
        </select>
        <button onClick={handleUnion}>Union</button>
      </div>

      <div>
        <h4>Deep Save</h4>
        <input
          type="text"
          placeholder="Alias for deep save"
          value={deepSaveAlias}
          onChange={(e) => setDeepSaveAlias(e.target.value)}
        />
        <button onClick={handleDeepSave}>Deep Save</button>
      </div>
    </div>
  );
}

export default ActionBar;
