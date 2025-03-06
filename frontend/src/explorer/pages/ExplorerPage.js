import React, { useState, useEffect } from "react";
import { Paper, Typography, Box } from "@mui/material";
import DataDisplay from "../components/DataDisplay";
import DataOperation from "../components/DataOperation";
import SnapshotManager from "../components/SnapshotManager";
import OperationHistory from "../components/OperationHistory";
import DataOverview from "../components/DataOverview";
import ExplorerService from "../services/ExplorerService";
import { useUser } from "../../UserContext";

function ExplorerPage() {
  const { userId } = useUser();
  const [data, setData] = useState([]);
  const [dataType, setDataType] = useState("universal_raw");
  const [dataOverview, setDataOverview] = useState(null);
  const [operationsHistory, setOperationsHistory] = useState([]);
  const [columns, setColumns] = useState([]);
  const [resetKey, setResetKey] = useState(0);

  useEffect(() => {
    if (!userId) return;
    const fetchData = async () => {
      try {
        const response = await ExplorerService.initUser(userId);
        // Expect response to be in the structure: 
        // { data, data_type, data_overview, operations_history }
        setData(response.data);
        setDataType(response.meta.data_type);
        setDataOverview(response.meta.data_overview);
        setOperationsHistory(response.meta.operations_history || []);
        setColumns(Object.keys(response.data[0] || {}));
      } catch (error) {
        console.error("Error fetching initial data:", error);
      }
    };
    fetchData();
  }, [userId]);

  const handleOperationApply = (result) => {
    // Expect result to have { data, data_type, data_overview, operations_history }
    setData(result.data);
    setDataType(result.meta.data_type);
    setDataOverview(result.meta.data_overview);
    setOperationsHistory(result.meta.operations_history || []);
    setColumns(Object.keys(result.data[0] || {}));
    setResetKey((prev) => prev + 1);
  };

  const handleSnapshotReset = (newData) => {
    // Expect newData to have { data, data_type, data_overview, operations_history }
    setData(newData.data);
    setDataType(newData.meta.data_type);
    setDataOverview(newData.meta.data_overview);
    setOperationsHistory(newData.meta.operations_history || []);
    setColumns(Object.keys(newData.data[0] || {}));
    setResetKey((prev) => prev + 1);
  };

  return (
    <Box sx={{ height: "100vh", padding: "1rem", flexDirection: "column", boxSizing: "border-box", overflow: "hidden" }}>
      {/* Top panel: Title & Snapshot Manager */}
      <Paper
        sx={{
          flex: "0 0 10vh", // Fixed height (10vh)
          display: "flex",
          justifyContent: "space-between",
          alignItems: "center",
          padding: "1rem",
          marginBottom: "1rem",
          height: "10%",
          backgroundColor: "#f5f5f5",
          borderRadius: "8px",
        }}
        elevation={3}
      >
        <Typography variant="h4" component="h1" sx={{ fontWeight: "bold" }}>
          Explorer
        </Typography>
        <SnapshotManager
          userId={userId}
          onSnapshotLoad={handleSnapshotReset}
          onReset={handleSnapshotReset}
        />
      </Paper>

      {/* Main content area: Two panels side-by-side */}
      <Box
        sx={{
          display: "flex",
          height: "85%", // Remaining vertical space after top panel
          gap: "1rem",
        }}
      >
        {/* Left panel: Data Operation & Operation History */}
        <Paper
          sx={{
            flex: 2,
            padding: "1rem",
            display: "flex",
            flexDirection: "column",
            height: "100%",
            backgroundColor: "#fafafa",
            borderRadius: "8px",
            overflow: "hidden"
          }}
          elevation={3}
        >
          {/* Data Operation Component */}
          <Box sx={{ flex: 2, overflow: "auto" }}>
            <DataOperation key={resetKey} userId={userId} onApplyOperation={handleOperationApply} />
          </Box>
          {/* Operation History Component */}
          <Box sx={{ flex: 1, marginTop: "1rem" }}>
            <OperationHistory operationsHistory={operationsHistory} />
          </Box>
        </Paper>

        {/* Right panel: Data Display & Data Overview */}
        <Paper
          sx={{
            flex: 3,
            padding: "1rem",
            display: "flex",
            flexDirection: "column",
            height: "100%",
            backgroundColor: "#fafafa",
            borderRadius: "8px",
            overflow: "hidden"
          }}
          elevation={3}
        >
          {/* Data Display: Takes up ~7/8 of the vertical space */}
          <Box sx={{ flex: 7, overflow: "auto", marginBottom: "1rem" }}>
            <DataDisplay data={data} dataType={dataType} />
          </Box>
          {/* Data Overview: Takes up ~1/8 of the vertical space */}
          <Box
            sx={{
              flex: 1,
              backgroundColor: "#e0e0e0",
              borderRadius: "4px",
              padding: "0.5rem",
              overflow: "auto"
            }}
          >
            <DataOverview dataType={dataType} dataOverview={dataOverview} />
          </Box>
        </Paper>
      </Box>
    </Box>
  );
}

export default ExplorerPage;
