import React, { useState, useEffect } from "react";
import { Paper, Typography } from "@mui/material";
import DataTable from "../components/DataTable";
import DataOperation from "../components/DataOperation";
import SnapshotManager from "../components/SnapshotManager";
import ExplorerService from "../services/ExplorerService";
import { useUser } from "../../UserContext";

function ExplorerPage() {
  const { userId } = useUser();
  const [data, setData] = useState([]);
  const [columns, setColumns] = useState([]);
  const [resetKey, setResetKey] = useState(0);

  useEffect(() => {
    if (!userId) return;
    const fetchData = async () => {
      try {
        const response = await ExplorerService.initUser(userId);
        setData(response);
        setColumns(Object.keys(response[0] || {}));
      } catch (error) {
        console.error("Error fetching initial data:", error);
      }
    };
    fetchData();
  }, [userId]);

  const handleOperationApply = (result) => {
    setData(result);
    setColumns(Object.keys(result[0] || {}));
  };

  const handleSnapshotReset = (newData) => {
    setData(newData);
    setColumns(Object.keys(newData[0] || {}));
    setResetKey((prev) => prev + 1);
  };

  return (
    <div style={{ padding: "1rem" }}>
      <div style={{ display: "flex", justifyContent: "space-between", alignItems: "center", marginBottom: "1rem" }}>
        <Typography variant="h4">Explorer</Typography>
        <SnapshotManager userId={userId} onSnapshotLoad={handleSnapshotReset} onReset={handleSnapshotReset} />
      </div>
      <DataOperation key={resetKey} userId={userId} onApplyOperation={handleOperationApply} />
      <Paper style={{ padding: "1rem" }}>
        <DataTable data={data} columns={columns} />
      </Paper>
    </div>
  );
}

export default ExplorerPage;