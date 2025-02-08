// src/pages/ExplorerPage.js
import React, { useState, useEffect } from "react";
import { Paper, Button, Modal, Box, Typography, Grid } from "@mui/material";
import { useUser } from "../../UserContext";

// Import the unified explorer service
import ExplorerService from "../services/ExplorerService";

// Import UI components
import DataTable from "../components/DataTable";
import CategoricalFilter from "../components/CategoricalFilter"; 
import DateFilter from "../components/DateFilter";
import SubtextFilter from "../components/SubtextFilter";
import TraversalOptions from "../components/TraversalOptions";
import SortOptions from "../components/SortOptions";
import SaveOptions from "../components/SaveOptions";
import MultiDatasetOperations from "../components/MultiDatasetOperations";
import FieldsFilterModal from "../components/FieldsFilterModal";

function ExplorerPage() {
  const { userId } = useUser();
  const [data, setData] = useState([]);
  const [columns, setColumns] = useState([]);
  const [showFieldsModal, setShowFieldsModal] = useState(false);
  const [savedAliases, setSavedAliases] = useState([]); // For saving operations

  useEffect(() => {
    if (!userId) return;
    const fetchData = async () => {
      console.log("fetchData called for initUser with userId:", userId);
      try {
        const response = await ExplorerService.initUser(userId);
        console.log("initUser response:", response);
        setData(response.data);
        setColumns(Object.keys(response.data[0] || {}));
      } catch (error) {
        console.error("Error fetching initial data:", error);
      }
    };
    fetchData();
  }, [userId]);

  // Handler for categorical filtering.
  const handleCategoricalFilter = async (column, value) => {
    console.log("handleCategoricalFilter called with:", { column, value });
    try {
      const response = await ExplorerService.filterData(userId, column, value, "=");
      console.log("Categorical filter response:", response);
      setData(response.data);
    } catch (error) {
      console.error("Error applying categorical filter:", error);
    }
  };

  // Handler for date filtering.
  const handleDateFilter = async (date, condition) => {
    console.log("handleDateFilter called with:", { date, condition });
    try {
      // Assuming explorerService.dateFilter exists; adjust as needed.
      const response = await ExplorerService.dateFilter(userId, { date, condition });
      console.log("Date filter response:", response);
      setData(response.data);
    } catch (error) {
      console.error("Error applying date filter:", error);
    }
  };

  // Handler for subtext filtering.
  const handleSubtextFilter = async (column, text) => {
    console.log("handleSubtextFilter called with:", { column, text });
    try {
      // Assuming explorerService.subtextFilter exists; adjust as needed.
      const response = await ExplorerService.subtextFilter(userId, { column, text });
      console.log("Subtext filter response:", response);
      setData(response.data);
    } catch (error) {
      console.error("Error applying subtext filter:", error);
    }
  };

  // Handler for traversals.
  const handleTraversal = async (direction) => {
    console.log("handleTraversal called with direction:", direction);
    try {
      // Adjust the parameters as needed; using "someStartId" as a placeholder.
      const response = await ExplorerService.traverseData(userId, "someStartId", direction);
      console.log("Traversal response:", response);
      setData(response.data);
    } catch (error) {
      console.error("Error performing traversal:", error);
    }
  };

  // Handler for sorting.
  const handleSort = async (column, order) => {
    console.log("handleSort called with:", { column, order });
    try {
      const response = await ExplorerService.sortData(userId, column, order);
      console.log("Sort response:", response);
      setData(response.data);
    } catch (error) {
      console.error("Error sorting data:", error);
    }
  };

  // Handler for saving datasets.
  const handleSave = async (saveType, alias) => {
    console.log("handleSave called with:", { saveType, alias });
    try {
      if (saveType === "temp") {
        await ExplorerService.tempSave(userId, data, alias);
      } else if (saveType === "deep") {
        await ExplorerService.deepSave(userId, data, alias);
      }
      console.log(`${saveType} save successful with alias:`, alias);
      setSavedAliases([...savedAliases, alias]);
    } catch (error) {
      console.error(`Error during ${saveType} save:`, error);
    }
  };

  // Handler for multi-dataset operations.
  const handleMultiDatasetOperation = async (operationType, params) => {
    console.log("handleMultiDatasetOperation called with:", { operationType, params });
    try {
      const response = await ExplorerService.multiDatasetOperation(userId, operationType, params);
      console.log("Multi-dataset operation response:", response);
      setData(response.data);
    } catch (error) {
      console.error("Error performing multi-dataset operation:", error);
    }
  };

  return (
    <div style={{ padding: "1rem" }}>
      <Typography variant="h4" gutterBottom>
        Explorer
      </Typography>

      {/* Toolbar / Operations Section */}
      <Paper style={{ padding: "1rem", marginBottom: "1rem" }}>
        <Grid container spacing={2}>
          {/* Group 1: Filters */}
          <Grid item xs={12} md={4}>
            <CategoricalFilter
              columns={["functionalities", "subject matters", "general categories", "tags"]}
              onApply={handleCategoricalFilter}
            />
          </Grid>
          <Grid item xs={12} md={4}>
            <DateFilter onApply={handleDateFilter} />
          </Grid>
          <Grid item xs={12} md={4}>
            <SubtextFilter columns={["text", "title"]} onApply={handleSubtextFilter} />
          </Grid>

          {/* Group 2: Fields Filtering (Modal Trigger) */}
          <Grid item xs={12} md={4}>
            <Button
              variant="contained"
              onClick={() => {
                console.log("Opening Fields Filter Modal");
                setShowFieldsModal(true);
              }}
            >
              Fields Filter
            </Button>
          </Grid>

          {/* Group 3: Traversals */}
          <Grid item xs={12} md={4}>
            <TraversalOptions onTraverse={handleTraversal} />
          </Grid>

          {/* Group 4: Sorting */}
          <Grid item xs={12} md={4}>
            <SortOptions columns={columns} onSort={handleSort} />
          </Grid>

          {/* Group 5: Saving & Multi-Dataset Operations */}
          <Grid item xs={12}>
            <SaveOptions onSave={handleSave} />
          </Grid>
          <Grid item xs={12}>
            <MultiDatasetOperations onOperate={handleMultiDatasetOperation} />
          </Grid>
        </Grid>
      </Paper>

      {/* Data Table Section */}
      <Paper style={{ padding: "1rem" }}>
        <DataTable data={data} columns={columns} />
      </Paper>

      {/* Modal for Fields Filtering */}
      <Modal
        open={showFieldsModal}
        onClose={() => {
          console.log("Closing Fields Filter Modal");
          setShowFieldsModal(false);
        }}
      >
        <Box
          sx={{
            position: "absolute",
            top: "50%",
            left: "50%",
            transform: "translate(-50%, -50%)",
            width: 400,
            bgcolor: "background.paper",
            p: 4,
            boxShadow: 24,
          }}
        >
          <FieldsFilterModal
            onApply={(filterParams) => {
              console.log("Fields Filter applied with parameters:", filterParams);
              // Here you could call explorerService.fieldsFilter(userId, filterParams) if defined.
              setShowFieldsModal(false);
            }}
          />
        </Box>
      </Modal>
    </div>
  );
}

export default ExplorerPage;
