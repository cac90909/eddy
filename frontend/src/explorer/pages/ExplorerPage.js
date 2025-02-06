// src/pages/ExplorerPage.js
import React, { useState, useEffect } from "react";
import { Paper, Button, Modal, Box, Typography } from "@mui/material";
import { Grid } from "@mui/material/Grid"; // If giving issues, try using Grid 2 from "@mui/material"
import { useUser } from "../../UserContext";

// Import our React services
import explorerService from "../services/explorerService";
import snapshotService from "../services/snapshotService";
import cacheService from "../services/cacheService";

// Import UI components (these should be implemented separately)
import DataTable from "../components/DataTable";
import CategoricalFilter from "../../../dep - explorer/CategoricalFilter";
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
        const response = await explorerService.initUser(userId);
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
      const response = await explorerService.filterData(userId, column, value, "=");
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
      const response = await explorerService.dateFilter(userId, { date, condition });
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
      const response = await explorerService.subtextFilter(userId, { column, text });
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
      // Adjust the parameters as needed (e.g., startId can be part of operationParams)
      const response = await explorerService.traverseData(userId, "someStartId", direction);
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
      const response = await explorerService.sortData(userId, column, order);
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
        await explorerService.tempSave(userId, data, alias);
      } else if (saveType === "deep") {
        await explorerService.deepSave(userId, data, alias);
      }
      console.log(`${saveType} save successful with alias:`, alias);
      // Optionally update savedAliases state here.
      setSavedAliases([...savedAliases, alias]);
    } catch (error) {
      console.error(`Error during ${saveType} save:`, error);
    }
  };

  // Handler for multi-dataset operations.
  const handleMultiDatasetOperation = async (operationType, params) => {
    console.log("handleMultiDatasetOperation called with:", { operationType, params });
    try {
      const response = await explorerService.multiDatasetOperation(userId, operationType, params);
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
              // You could call explorerService.fieldsFilter(userId, filterParams) here.
              setShowFieldsModal(false);
            }}
          />
        </Box>
      </Modal>
    </div>
  );
}

export default ExplorerPage;


// import React, { useState, useEffect, useRef } from "react";
// import { useUser } from "../../UserContext";
// import explorerService from "../services/explorerService";
// import DataTable from "../components/DataTable";
// import FilterPanel from "../components/FilterPanel";
// import TraversalPanel from "../components/TraversalPanel";
// import SortPanel from "../components/SortPanel";
// import ActionBar from "../components/ActionBar";

// function ExplorerPage() {
//   const { userId } = useUser();
//   const [data, setData] = useState([]);
//   const [columns, setColumns] = useState([]);
//   const [savedAliases, setSavedAliases] = useState([]); // Temp saves
//   const isFetching = useRef(false);

//   useEffect(() => {
//     if (!userId) return;

//     const fetchData = async () => {
//       if (isFetching.current) return;
//       isFetching.current = true;

//       try {
//         const response = await explorerService.initUser(userId);
//         setData(response.data);
//         setColumns(Object.keys(response.data[0] || {}));
//       } catch (error) {
//         console.error("Error fetching data:", error);
//       } finally {
//         isFetching.current = false;
//       }
//     };

//     fetchData();
//   }, [userId]);

//   // Callbacks for ActionBar
//   const handlePrevious = async () => {
//     try {
//       const response = await explorerService.previousData(userId);
//       setData(response.data);
//     } catch (error) {
//       console.error("Error fetching previous dataset:", error);
//     }
//   };

//   const handleTempSave = async () => {
//     try {
//       const alias = `Temp Save ${savedAliases.length + 1}`;
//       await explorerService.tempSave(data, alias);
//       setSavedAliases([...savedAliases, alias]); // Update saved aliases
//     } catch (error) {
//       console.error("Error saving dataset:", error);
//     }
//   };

//   const handleUnion = async (alias) => {
//     try {
//       const response = await explorerService.unionData(userId, alias);
//       setData(response.data);
//     } catch (error) {
//       console.error("Error performing union:", error);
//     }
//   };

//   const handleDeepSave = async (alias) => {
//     try {
//       await explorerService.deepSave(data, alias);
//       console.log("Dataset deep saved with alias:", alias);
//     } catch (error) {
//       console.error("Error deep saving dataset:", error);
//     }
//   };

//   const handleSort = async (sortParams) => {
//     try {
//       const response = await explorerService.sortData(sortParams.columnName, sortParams.sortOrder);
//       setData(response.data);
//     } catch (error) {
//       console.error("Error applying sort:", error);
//     }
//   };

//   if (!userId) return <div>No User ID Provided</div>;

//   return (
//     <div>
//       <h1>Explorer</h1>
//       <div style={{ display: "flex", gap: "1rem" }}>
//         <div style={{ flex: "1", maxWidth: "300px" }}>
//           <FilterPanel onApplyFilter={(filter) => console.log("Filter applied:", filter)} columns={columns} />
//           <TraversalPanel />
//           <SortPanel onSort={handleSort} />
//         </div>
//         <div style={{ flex: "3" }}>
//           <ActionBar
//             onPrevious={handlePrevious}
//             onTempSave={handleTempSave}
//             onUnion={handleUnion}
//             onDeepSave={handleDeepSave}
//             savedAliases={savedAliases}
//           />
//           <DataTable data={data} columns={columns} />
//         </div>
//       </div>
//     </div>
//   );
// }

// export default ExplorerPage;
