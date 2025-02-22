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
  const [dataAmount, setDataAmount] = useState(null);
  const [columns, setColumns] = useState([]);
  const [resetKey, setResetKey] = useState(0);

  useEffect(() => {
    if (!userId) return;
    const fetchData = async () => {
      try {
        const response = await ExplorerService.initUser(userId);
        // Expect response to be in the structure { data, data_type, data_amount }
        setData(response.data);
        setDataType(response.data_type);
        setDataAmount(response.data_amount);
        setColumns(Object.keys(response.data[0] || {}));
      } catch (error) {
        console.error("Error fetching initial data:", error);
      }
    };
    fetchData();
  }, [userId]);

  const handleOperationApply = (result) => {
    // Expect result to have { data, data_type, data_amount }
    setData(result.data);
    setDataType(result.data_type);
    setDataAmount(result.data_amount);
    setColumns(Object.keys(result.data[0] || {}));
    setResetKey((prev) => prev + 1);
  };

  const handleSnapshotReset = (newData) => {
    setData(newData.data);
    setDataType(newData.data_type);
    setDataAmount(newData.data_amount);
    setColumns(Object.keys(newData.data[0] || {}));
    setResetKey((prev) => prev + 1);
  };

  return (
    <Box sx={{ height: "100vh", padding: "1rem", boxSizing: "border-box" }}>
      {/* Top panel: Title & Snapshot Manager */}
      <Paper
        sx={{
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
          height: "88%", // Remaining vertical space after top panel
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
          }}
          elevation={3}
        >
          {/* Data Operation Component */}
          <Box sx={{ flex: 2 }}>
            <DataOperation key={resetKey} userId={userId} onApplyOperation={handleOperationApply} />
          </Box>
          {/* Operation History Component */}
          <Box sx={{ flex: 1, marginTop: "1rem" }}>
            <OperationHistory userId={userId} refreshKey={resetKey} />
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
            }}
          >
            <DataOverview dataType={dataType} dataAmount={dataAmount} />
          </Box>
        </Paper>
      </Box>
    </Box>
  );
}

export default ExplorerPage;
