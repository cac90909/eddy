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
import ExplorerService from "../services/ExplorerService";
import { useUser } from "../../contexts/UserSessionContext";
import { useSetOperationResult } from "../hooks/useSetOperationResult";
import { useApplyOperation } from "../hooks/useApplyOperation";
import { useExplorerConfig } from "../../contexts/ExplorerConfigContext";

function ExplorerPage() {
  const { userId } = useUser();
  const { applyOperation, loading, error } = useApplyOperation(userId);
  const { operationResult, setOperationResult } = useSetOperationResult();
  const { explorerConfig, setExplorerConfig } = useExplorerConfig();


  const handleApplyOperation = async (operationName, operationArguments, updateGlobal = true) => {
    try {
      const operationResult = await applyOperation(operationName, operationArguments);
      if (updateGlobal) {
        setOperationResult(operationResult);
      }
      return operationResult;
    } catch (err) {
      console.error("Operation failed:", err);
      throw err;
    }
  };

  //sometimes we need to do an operation, but not set operation result because the result isnt data pertaining to the data display


  useEffect(() => {
    const initializeSession = async () => {
      if (userId) {
        // Call start_explorer_session and save the returned configuration in context
        const explorerConfig = await applyOperation(operationName="start_explorer_session", operationArguments={});
        setExplorerConfig(explorerConfig);
        
        const fullData = await applyOperation(operationName="get_full_data", operationArguments={});
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
        <SnapshotManager userId={userId} /* other props */ />
      </TopPanel>
      <MainContentContainer>
        <LeftPanel elevation={3}>
          <OperationNavigation userId={userId} applyOperation={handleApplyOperation}  />
          <OperationHistory /* operationsHistory={sessionData.operationsHistory} */ />
        </LeftPanel>
        <RightPanel elevation={3}>
          <DataDisplayContainer>
            <DataDisplay /* data={sessionData.data} dataType={sessionData.dataType} */ />
          </DataDisplayContainer>
          <DataOverviewContainer>
            <DataOverview /* dataType={sessionData.dataType} dataOverview={sessionData.dataOverview} */ />
          </DataOverviewContainer>
        </RightPanel>
      </MainContentContainer>
    </ExplorerContainer>
  );
}

export default ExplorerPage;


