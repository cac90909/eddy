import React, { useState, useEffect, useRef } from "react";
import { useUser } from "../../UserContext";
import explorerService from "../services/explorerService";
import DataTable from "../components/DataTable";
import FilterPanel from "../components/FilterPanel";
import TraversalPanel from "../components/TraversalPanel";
import SortPanel from "../components/SortPanel";
import ActionBar from "../components/ActionBar";

function ExplorerPage() {
  const { userId } = useUser();
  const [data, setData] = useState([]);
  const [columns, setColumns] = useState([]);
  const [savedAliases, setSavedAliases] = useState([]); // Temp saves
  const isFetching = useRef(false);

  useEffect(() => {
    if (!userId) return;

    const fetchData = async () => {
      if (isFetching.current) return;
      isFetching.current = true;

      try {
        const response = await explorerService.initUser(userId);
        setData(response.data);
        setColumns(Object.keys(response.data[0] || {}));
      } catch (error) {
        console.error("Error fetching data:", error);
      } finally {
        isFetching.current = false;
      }
    };

    fetchData();
  }, [userId]);

  // Callbacks for ActionBar
  const handlePrevious = async () => {
    try {
      const response = await explorerService.previousData(userId);
      setData(response.data);
    } catch (error) {
      console.error("Error fetching previous dataset:", error);
    }
  };

  const handleTempSave = async () => {
    try {
      const alias = `Temp Save ${savedAliases.length + 1}`;
      await explorerService.tempSave(data, alias);
      setSavedAliases([...savedAliases, alias]); // Update saved aliases
    } catch (error) {
      console.error("Error saving dataset:", error);
    }
  };

  const handleUnion = async (alias) => {
    try {
      const response = await explorerService.unionData(userId, alias);
      setData(response.data);
    } catch (error) {
      console.error("Error performing union:", error);
    }
  };

  const handleDeepSave = async (alias) => {
    try {
      await explorerService.deepSave(data, alias);
      console.log("Dataset deep saved with alias:", alias);
    } catch (error) {
      console.error("Error deep saving dataset:", error);
    }
  };

  const handleSort = async (sortParams) => {
    try {
      const response = await explorerService.sortData(sortParams.columnName, sortParams.sortOrder);
      setData(response.data);
    } catch (error) {
      console.error("Error applying sort:", error);
    }
  };

  if (!userId) return <div>No User ID Provided</div>;

  return (
    <div>
      <h1>Explorer</h1>
      <div style={{ display: "flex", gap: "1rem" }}>
        <div style={{ flex: "1", maxWidth: "300px" }}>
          <FilterPanel onApplyFilter={(filter) => console.log("Filter applied:", filter)} columns={columns} />
          <TraversalPanel />
          <SortPanel onSort={handleSort} />
        </div>
        <div style={{ flex: "3" }}>
          <ActionBar
            onPrevious={handlePrevious}
            onTempSave={handleTempSave}
            onUnion={handleUnion}
            onDeepSave={handleDeepSave}
            savedAliases={savedAliases}
          />
          <DataTable data={data} columns={columns} />
        </div>
      </div>
    </div>
  );
}

export default ExplorerPage;
