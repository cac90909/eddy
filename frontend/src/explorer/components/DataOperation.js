import React, { useState, useEffect } from "react";
import {
  Box,
  Button,
  FormControl,
  InputLabel,
  MenuItem,
  Select,
  TextField,
  Checkbox,
  FormControlLabel,
  Grid,
  Typography,
} from "@mui/material";
import ExplorerService from "../services/ExplorerService";

// Define categorical columns that should use unique filter values.
const categoricalColumns = [
  "functionalities",
  "subject matters",
  "general categories",
  "tags",
  "parents_ids",
  "children_ids",
  "siblings_ids",
];

const operationTypes = ["filter", "traverse", "sort", "union"];

const filterOperatorsByColumn = {
  id: ["=", "!="],
  entry_id: ["=", "!="],
  date: [">", "<", "=", "!="],
  title: ["string_contains"],
  text: ["string_contains"],
  functionalities: ["array_contains", "array_not_contains"],
  "subject matters": ["array_contains", "array_not_contains"],
  "general categories": ["array_contains", "array_not_contains"],
  tags: ["array_contains", "array_not_contains"],
  parents_ids: ["array_contains", "array_not_contains"],
  children_ids: ["array_contains", "array_not_contains"],
  siblings_ids: ["array_contains", "array_not_contains"],
};

const DataOperation = ({ userId, onApplyOperation }) => {
  const [operationType, setOperationType] = useState("");
  const [column, setColumn] = useState("");
  const [operator, setOperator] = useState("");
  const [value, setValue] = useState("");
  const [jsonKey, setJsonKey] = useState("");
  const [availableOperators, setAvailableOperators] = useState([]);
  const [traverseOptions, setTraverseOptions] = useState({
    horizontal: false,
    upwards: false,
    downwards: false,
  });
  const [sortOrder, setSortOrder] = useState("asc");
  const [uniqueJsonKeys, setUniqueJsonKeys] = useState([]);
  const [uniqueColumnValues, setUniqueColumnValues] = useState([]);

  // For traverse: store unique entry_ids and the chosen start_id.
  const [uniqueEntryIds, setUniqueEntryIds] = useState([]);
  const [startId, setStartId] = useState("");

  useEffect(() => {
    if (operationType === "filter") {
      setValue("");
      setUniqueColumnValues([]);
      if (column === "fields") {
        ExplorerService.getUniqueJsonKeys(userId)
          .then((keys) => setUniqueJsonKeys(keys))
          .catch((err) => console.error("Error fetching JSON keys:", err));
      } else if (categoricalColumns.includes(column)) {
        ExplorerService.getUniqueColumnValues(userId, column)
          .then((values) => setUniqueColumnValues(values))
          .catch((err) => console.error("Error fetching unique column values:", err));
      }
      setAvailableOperators(filterOperatorsByColumn[column] || ["=", "!="]);
    }
  }, [operationType, column, userId]);

  useEffect(() => {
    if (operationType === "traverse") {
      ExplorerService.getUniqueColumnValues(userId, "entry_id")
        .then((ids) => setUniqueEntryIds(ids))
        .catch((err) => console.error("Error fetching unique entry_ids:", err));
    }
  }, [operationType, userId]);

  const isApplyEnabled = () => {
    if (!operationType) return false;
    if (operationType === "filter") {
      if (!column || !operator) return false;
      if (categoricalColumns.includes(column)) return value !== "";
      if (column === "fields") return jsonKey !== "" && value !== "";
      return value !== "";
    } else if (operationType === "traverse") {
      return startId !== "" && Object.values(traverseOptions).some((v) => v);
    } else if (operationType === "sort") {
      if (!column) return false;
      if (column === "fields") return jsonKey !== "";
      return true;
    } else if (operationType === "union") {
      return true;
    }
    return false;
  };

  const handleApply = async () => {
    let params = {};
    if (operationType === "filter") {
      params = column === "fields" 
        ? { column_name: column, json_key: jsonKey, filter_value: value, filter_type: operator }
        : { column_name: column, filter_value: value, filter_type: operator };
    } else if (operationType === "traverse") {
      const traversal_directions = Object.keys(traverseOptions).filter((dir) => traverseOptions[dir]);
      if (traversal_directions.length === 0) {
        alert("Please select at least one traversal direction.");
        return;
      }
      params = { start_id: startId, traversal_directions: traversal_directions };
    } else if (operationType === "sort") {
      params = { column_name: column, sort_order: sortOrder };
      if (column === "fields") {
        params.json_key = jsonKey;
      }
    } else if (operationType === "union") {
      params = { note: "union operation not implemented" };
    }
    try {
      const result = await ExplorerService.handleOperation(userId, operationType, params);
      onApplyOperation(result);
    } catch (error) {
      console.error("Error applying operation:", error);
    }
  };

  return (
    <Box sx={{ p: 2, border: "1px solid #ccc", borderRadius: 2, mb: 2 }}>
      <Typography variant="h5" gutterBottom>
        Data Operation
      </Typography>
      <Grid container spacing={2}>
        <Grid item xs={12} sm={6}>
          <FormControl fullWidth>
            <InputLabel id="operation-type-label">Operation Type</InputLabel>
            <Select
              labelId="operation-type-label"
              value={operationType}
              label="Operation Type"
              onChange={(e) => {
                setOperationType(e.target.value);
                setColumn("");
                setOperator("");
                setValue("");
                setJsonKey("");
                setStartId("");
              }}
            >
              {operationTypes.map((op) => (
                <MenuItem key={op} value={op}>
                  {op.charAt(0).toUpperCase() + op.slice(1)}
                </MenuItem>
              ))}
            </Select>
          </FormControl>
        </Grid>
        {operationType === "filter" && (
          <>
            <Grid item xs={12} sm={6}>
              <FormControl fullWidth>
                <InputLabel id="filter-column-label">Filter Column</InputLabel>
                <Select
                  labelId="filter-column-label"
                  value={column}
                  label="Filter Column"
                  onChange={(e) => {
                    setColumn(e.target.value);
                    setOperator("");
                    setValue("");
                    setJsonKey("");
                  }}
                >
                  {[
                    "id",
                    "entry_id",
                    "date",
                    "title",
                    "text",
                    "functionalities",
                    "subject matters",
                    "general categories",
                    "tags",
                    "parents_ids",
                    "children_ids",
                    "siblings_ids",
                    "fields",
                  ].map((col) => (
                    <MenuItem key={col} value={col}>
                      {col}
                    </MenuItem>
                  ))}
                </Select>
              </FormControl>
            </Grid>
            {column === "fields" && (
              <Grid item xs={12}>
                <FormControl fullWidth>
                  <InputLabel id="json-key-label">JSON Key</InputLabel>
                  <Select
                    labelId="json-key-label"
                    value={jsonKey}
                    label="JSON Key"
                    onChange={(e) => {
                      setJsonKey(e.target.value);
                      setAvailableOperators(["string_contains", "=", "!="]);
                    }}
                  >
                    {uniqueJsonKeys.map((key) => (
                      <MenuItem key={key} value={key}>
                        {key}
                      </MenuItem>
                    ))}
                  </Select>
                </FormControl>
              </Grid>
            )}
            <Grid item xs={12} sm={6}>
              <FormControl fullWidth>
                <InputLabel id="filter-operator-label">Operator</InputLabel>
                <Select
                  labelId="filter-operator-label"
                  value={operator}
                  label="Operator"
                  onChange={(e) => setOperator(e.target.value)}
                >
                  {availableOperators.map((op) => (
                    <MenuItem key={op} value={op}>
                      {op}
                    </MenuItem>
                  ))}
                </Select>
              </FormControl>
            </Grid>
            <Grid item xs={12} sm={6}>
              {categoricalColumns.includes(column) && uniqueColumnValues.length ? (
                <FormControl fullWidth>
                  <InputLabel id="filter-value-label">Select Value</InputLabel>
                  <Select
                    labelId="filter-value-label"
                    value={value}
                    label="Select Value"
                    onChange={(e) => setValue(e.target.value)}
                  >
                    {uniqueColumnValues.map((opt) => (
                      <MenuItem key={opt} value={opt}>
                        {opt}
                      </MenuItem>
                    ))}
                  </Select>
                </FormControl>
              ) : (
                <>
                  {column === "date" ? (
                    <TextField
                      fullWidth
                      label="Enter Date (YYYY-MM-DD)"
                      value={value}
                      onChange={(e) => setValue(e.target.value)}
                    />
                  ) : (
                    <TextField
                      fullWidth
                      label="Filter Value"
                      value={value}
                      onChange={(e) => setValue(e.target.value)}
                    />
                  )}
                </>
              )}
            </Grid>
          </>
        )}
        {operationType === "traverse" && (
          <>
            <Grid item xs={12} sm={6}>
              <FormControl fullWidth>
                <InputLabel id="traverse-start-id-label">Start ID</InputLabel>
                <Select
                  labelId="traverse-start-id-label"
                  value={startId}
                  label="Start ID"
                  onChange={(e) => setStartId(e.target.value)}
                  MenuProps={{
                    PaperProps: { style: { maxHeight: 200 } },
                  }}
                >
                  {uniqueEntryIds.map((id) => (
                    <MenuItem key={id} value={id}>
                      {id}
                    </MenuItem>
                  ))}
                </Select>
              </FormControl>
            </Grid>
            <Grid item xs={12}>
              <Typography variant="subtitle1">Select Traversal Directions</Typography>
              {["horizontal", "upwards", "downwards"].map((dir) => (
                <FormControlLabel
                  key={dir}
                  control={
                    <Checkbox
                      checked={traverseOptions[dir]}
                      onChange={(e) =>
                        setTraverseOptions({ ...traverseOptions, [dir]: e.target.checked })
                      }
                    />
                  }
                  label={dir.charAt(0).toUpperCase() + dir.slice(1)}
                />
              ))}
            </Grid>
          </>
        )}
        {operationType === "sort" && (
          <>
            <Grid item xs={12} sm={6}>
              <FormControl fullWidth>
                <InputLabel id="sort-column-label">Sort Column</InputLabel>
                <Select
                  labelId="sort-column-label"
                  value={column}
                  label="Sort Column"
                  onChange={(e) => {
                    setColumn(e.target.value);
                    setJsonKey("");
                    if (e.target.value === "fields") {
                      ExplorerService.getUniqueJsonKeys(userId)
                        .then((keys) => setUniqueJsonKeys(keys))
                        .catch((err) => console.error("Error fetching JSON keys:", err));
                    }
                  }}
                >
                  {[
                    "id",
                    "entry_id",
                    "date",
                    "title",
                    "text",
                    "functionalities",
                    "subject matters",
                    "general categories",
                    "tags",
                    "parents_ids",
                    "children_ids",
                    "siblings_ids",
                    "fields",
                  ].map((col) => (
                    <MenuItem key={col} value={col}>
                      {col}
                    </MenuItem>
                  ))}
                </Select>
              </FormControl>
            </Grid>
            {column === "fields" && (
              <Grid item xs={12} sm={6}>
                <FormControl fullWidth>
                  <InputLabel id="sort-json-key-label">JSON Key</InputLabel>
                  <Select
                    labelId="sort-json-key-label"
                    value={jsonKey}
                    label="JSON Key"
                    onChange={(e) => setJsonKey(e.target.value)}
                  >
                    {uniqueJsonKeys.map((key) => (
                      <MenuItem key={key} value={key}>
                        {key}
                      </MenuItem>
                    ))}
                  </Select>
                </FormControl>
              </Grid>
            )}
            <Grid item xs={12} sm={6}>
              <FormControl fullWidth>
                <InputLabel id="sort-order-label">Sort Order</InputLabel>
                <Select
                  labelId="sort-order-label"
                  value={sortOrder}
                  label="Sort Order"
                  onChange={(e) => setSortOrder(e.target.value)}
                >
                  <MenuItem value="asc">Ascending</MenuItem>
                  <MenuItem value="desc">Descending</MenuItem>
                </Select>
              </FormControl>
            </Grid>
          </>
        )}
        {operationType === "union" && (
          <Grid item xs={12}>
            <Typography variant="subtitle1">
              Union operation is currently a placeholder.
            </Typography>
          </Grid>
        )}
        <Grid item xs={12}>
          <Button variant="contained" color="primary" onClick={handleApply} disabled={!isApplyEnabled()}>
            Apply
          </Button>
        </Grid>
      </Grid>
    </Box>
  );
};

export default DataOperation;