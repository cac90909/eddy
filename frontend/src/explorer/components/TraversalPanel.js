import React from "react";

function TraversalPanel() {
  const handleTraversal = (type) => {
    console.log("Traversing:", type);
    // Call traversal API here
  };

  return (
    <div>
      <h3>Traversals</h3>
      <button onClick={() => handleTraversal("downward")}>Downward</button>
      <button onClick={() => handleTraversal("upward")}>Upward</button>
      <button onClick={() => handleTraversal("horizontal")}>Horizontal</button>
    </div>
  );
}

export default TraversalPanel;
