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
import DataOperation from "../components/DataOperation";
import SnapshotManager from "../components/SnapshotManager";
import OperationHistory from "../components/OperationHistory";
import DataOverview from "../components/DataOverview";
import ExplorerService from "../services/ExplorerService";
import { useUser } from "../../UserContext";
import { useSetOperationResult } from "../hooks/useSetOperationResult";
import { useApplyOperation } from "../hooks/useApplyOperation";

function ExplorerPage() {
  const { userId } = useUser();
  const { applyOperation, loading, error } = useApplyOperation(userId);
  const { operationResult, setOperationResult } = useSetOperationResult();

  const handleApplyOperation = async (operationName, operationArguments) => {
    try {
      const operationResult = await applyOperation(operationName, operationArguments);
      setOperationResult(operationResult);
    } catch (err) {
      console.error("Operation failed:", err);
    }
  };


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
          <DataOperation userId={userId} /* other props */ />
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


