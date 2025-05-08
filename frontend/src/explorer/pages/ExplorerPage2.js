import React, { useState, useEffect } from "react";
import { Paper, Typography, Box } from "@mui/material";
import {
  ExplorerContainer,
  TopPanel,
  MainContentContainer,
  LeftPanel,
  RightPanel,
  DataDisplayContainer,
  DataOverviewContainer
} from "./ExplorerPage.styles";
import DataDisplay from "../components/DataDisplay";
import OperationNavigation from "../components/OperationNavigation";
import SnapshotManager from "../components/SnapshotManager";
import OperationHistory from "../components/OperationHistory";
import DataOverview from "../components/DataOverview";
import { useUser } from "../../contexts/UserSessionContext";
import { useOperationHandler } from "../hooks/useOperationHandler"; // Import reusable hook

function ExplorerPage() {
  const { userId } = useUser();
  const [operationResult, setOperationResult] = useState(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);

  const handleApplyOperation = useOperationHandler(userId); // Hook gives you a reusable operation handler

  useEffect(() => {
    const initializeSession = async () => {
      if (!userId) return;

      try {
        // previously this returned a config object, and we set config from this call
        // now config (operation definitions) are stored in the FE util folder
        // right now, i don't know what this is returning or if it returns anything
        // it may just start the Django caches
        const resp = await handleApplyOperation("start_explorer_session", {}, false);

        const { data, meta } = await handleApplyOperation("get_full_data");
        setOperationResult({
          data,
          dataOverview: meta?.data_overview,
          operationsHistory: meta?.operations_history || [],
          columns: Object.keys(data?.[0] || {})
        });
      } catch (err) {
        console.error("Initialization failed", err);
        setError(err);
      }
    };

    initializeSession();
  }, [userId]);

  return (
    <ExplorerContainer>
      <TopPanel elevation={3}>
        <Typography variant="h4" component="h1" sx={{ fontWeight: "bold" }}>
          Explorer
        </Typography>
        <SnapshotManager userId={userId} />
      </TopPanel>
      <MainContentContainer>
        <LeftPanel elevation={3}>
          <OperationNavigation 
            userId={userId} 
            applyOperation={handleApplyOperation} 
            opConfigs={navigationOperations}  // Pass only the navigation operations
          />
          <OperationHistory />
        </LeftPanel>
        <RightPanel elevation={3}>
          <DataDisplayContainer>
            <DataDisplay />
          </DataDisplayContainer>
          <DataOverviewContainer>
            <DataOverview />
          </DataOverviewContainer>
        </RightPanel>
      </MainContentContainer>
    </ExplorerContainer>
  );
}

export default ExplorerPage;
