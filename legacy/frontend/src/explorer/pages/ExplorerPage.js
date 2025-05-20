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
      const result = await applyOperation(operationName, operationArguments);
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

  // Use useMemo to derive subsets of the config for child components.
  const navigationOperations = useMemo(() => {
    return explorerConfig?.operations_config.filter(op => op.display === "operation_navigation") || [];
  }, [explorerConfig]);

  const resetOperation = useMemo(() => {
    return explorerConfig?.operations_config.find(op => op.display === "reset_button");
  }, [explorerConfig]);

  const saveOperation = useMemo(() => {
    return explorerConfig?.operations_config.find(op => op.display === "save_button");
  }, [explorerConfig]);

  const deleteOperation = useMemo(() => {
    return explorerConfig?.operations_config.find(op => op.display === "delete_button");
  }, [explorerConfig]);

  const loadOperation = useMemo(() => {
    return explorerConfig?.operations_config.find(op => op.display === "load_button");
  }, [explorerConfig]);

  const loadOptionsOperation = useMemo(() => {
    return explorerConfig?.operations_config.find(op => op.display === "load_options");
  }, [explorerConfig]);

  const argumentOptionsOperation = useMemo(() => {
    return explorerConfig?.operations_config.find(op => op.display === "operation_argument_options");
  }, [explorerConfig]);

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
