import React, { useState, useEffect, useMemo } from "react";
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
import ExplorerService from "../services/ExplorerService2";
import { useUser } from "../../contexts/UserSessionContext";
import { useSetOperationResult } from "../hooks/useSessionData";
import { OPERATION_CONFIG } from "../config/OperationConfig";

function ExplorerPage() {
  const { userId } = useUser();
  const { operationResult, setOperationResult } = useSetOperationResult();
  const [loading, setLoading] = useState(false);
  const [error, setError]     = useState(null);

  const handleApplyOperation = async (operationName, operationArguments, updateGlobal = true) => {
    const operationConfig = OPERATION_CONFIG[operationKey];
    if (!operationConfig) {
      throw new Error(`Unknown operation: ${operationKey}`);
    }
    httpMethod = operationConfig["httpMethod"]
    try {
      const result = await ExplorerService.callOperation({operationName: operationName, operationArguments:operationArguments, userId:userId, httpMethod:httpMethod});
      //Left off here
      if (updateGlobal) {
        setOperationResult(result);
      }
      return result;
    } catch (err) {
      console.error("Operation failed:", err);
      throw err;
    }
  };

  // Initialize session on mount
  useEffect(() => {
    const initializeSession = async () => {
      if (userId) {
        // Call start_explorer_session and save the returned configuration in context
        const config = await applyOperation("start_explorer_session", {});
        setExplorerConfig(config);

        const fullData = await applyOperation("get_full_data", {});
        setOperationResult(fullData);
      }
    };
    initializeSession();
  }, [userId, applyOperation, setExplorerConfig]);


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
