// ExplorerSessionContext.js
import React, { createContext, useState, useContext } from "react";

const ExplorerConfigContext = createContext();

export const ExplorerConfigProvider = ({ children }) => {
  const [explorerConfig, setExplorerConfig] = useState(null);

  return (
    <ExplorerConfigContext.Provider value={{ explorerConfig, setExplorerConfig }}>
      {children}
    </ExplorerConfigContext.Provider>
  );
};

export const useExplorerConfig = () => {
  return useContext(ExplorerConfigContext);
};
