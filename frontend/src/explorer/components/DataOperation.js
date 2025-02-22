import React, { useState, useEffect } from "react";
import {
  Box,
  Button,
  FormControl,
  InputLabel,
  MenuItem,
  Select,
  TextField,
  Grid,
  Typography,
} from "@mui/material";
import ExplorerService from "../services/ExplorerService";
import RawOperationsConfig from "./RawOperationsConfig";
import MetricOperationsConfig from "./MetricOperationsConfig";
import ListOperationsConfig from "./ListOperationsConfig";
import EnrichmentOperationsConfig from "./EnrichmentOperationsConfig";

// Define operation categories with their labels, operations, and configuration.
const operationCategories = {
  raw: {
    label: "Raw",
    operations: Object.keys(RawOperationsConfig),
    config: RawOperationsConfig,
  },
  enriched: {
    label: "Enriched",
    operations: Object.keys(EnrichmentOperationsConfig),
    config: EnrichmentOperationsConfig,
  },
  metric: {
    label: "Metric",
    operations: Object.keys(MetricOperationsConfig),
    config: MetricOperationsConfig,
  },
  list: {
    label: "List",
    operations: Object.keys(ListOperationsConfig),
    config: ListOperationsConfig,
  },
};

const DataOperation = ({ userId, onApplyOperation }) => {
  const [selectedCategory, setSelectedCategory] = useState("raw");
  const [selectedOperation, setSelectedOperation] = useState("");
  const [formData, setFormData] = useState({});
  const [currentConfig, setCurrentConfig] = useState(null);
  // currentData holds dynamic options (e.g., unique JSON keys, unique column values, entry IDs)
  const [currentData, setCurrentData] = useState({
    uniqueJsonKeys: [],
    uniqueColumnValues: [],
    uniqueEntryIds: [],
  });

  // Load configuration when category or operation changes.
  useEffect(() => {
    if (
      selectedCategory &&
      selectedOperation &&
      operationCategories[selectedCategory]
    ) {
      const config =
        operationCategories[selectedCategory].config[selectedOperation];
      setCurrentConfig(config);
      setFormData({});
    }
  }, [selectedCategory, selectedOperation]);

  // Fetch dynamic data for raw, metric, list, and enriched operations.
  useEffect(() => {
    if (selectedCategory === "raw") {
      if (selectedOperation === "filter") {
        if (formData.column) {
          if (formData.column === "fields") {
            ExplorerService.getUniqueJsonKeys(userId)
              .then((keys) =>
                setCurrentData((prev) => ({ ...prev, uniqueJsonKeys: keys }))
              )
              .catch((err) => console.error("Error fetching JSON keys:", err));
          } else {
            const categoricalColumns = [
              "functionalities",
              "subject matters",
              "general categories",
              "tags",
              "parents_ids",
              "children_ids",
              "siblings_ids",
            ];
            if (categoricalColumns.includes(formData.column)) {
              ExplorerService.getUniqueColumnValues(userId, formData.column)
                .then((values) =>
                  setCurrentData((prev) => ({
                    ...prev,
                    uniqueColumnValues: values,
                  }))
                )
                .catch((err) =>
                  console.error("Error fetching unique column values:", err)
                );
            }
          }
        }
      } else if (selectedOperation === "traverse") {
        ExplorerService.getUniqueColumnValues(userId, "entry_id")
          .then((ids) =>
            setCurrentData((prev) => ({ ...prev, uniqueEntryIds: ids }))
          )
          .catch((err) =>
            console.error("Error fetching unique entry IDs:", err)
          );
      }
    } else if (selectedCategory === "metric") {
      if (formData.column === "fields") {
        ExplorerService.getUniqueJsonKeys(userId)
          .then((keys) =>
            setCurrentData((prev) => ({ ...prev, uniqueJsonKeys: keys }))
          )
          .catch((err) => console.error("Error fetching JSON keys:", err));
      }
    } else if (selectedCategory === "list") {
      if (formData.column === "fields") {
        ExplorerService.getUniqueJsonKeys(userId)
          .then((keys) =>
            setCurrentData((prev) => ({ ...prev, uniqueJsonKeys: keys }))
          )
          .catch((err) => console.error("Error fetching JSON keys:", err));
      } else if (formData.column && formData.column !== "fields") {
        ExplorerService.getUniqueColumnValues(userId, formData.column)
          .then((values) =>
            setCurrentData((prev) => ({ ...prev, uniqueColumnValues: values }))
          )
          .catch((err) =>
            console.error("Error fetching unique column values:", err)
          );
      }
    } else if (selectedCategory === "enriched") {
      // For group_aggregate, if group_column or target_column is "fields", fetch JSON keys.
      if (selectedOperation === "group_aggregate") {
        if (
          (formData.group_column && formData.group_column === "fields") ||
          (formData.target_column && formData.target_column === "fields")
        ) {
          ExplorerService.getUniqueJsonKeys(userId)
            .then((keys) =>
              setCurrentData((prev) => ({ ...prev, uniqueJsonKeys: keys }))
            )
            .catch((err) => console.error("Error fetching JSON keys:", err));
        }
      }
    }
  }, [selectedCategory, selectedOperation, formData.column, formData.group_column, formData.target_column, userId]);

  const handleFieldChange = (key, value) => {
    setFormData((prev) => ({ ...prev, [key]: value }));
  };

  // Render a form field based on the current configuration.
  const renderField = (field) => {
    if (field.displayCondition && !field.displayCondition(formData)) return null;
    const fieldType =
      typeof field.type === "function" ? field.type(formData) : field.type;
    let options = [];
    if (fieldType === "select" || fieldType === "multiselect") {
      options =
        typeof field.options === "function"
          ? field.options(formData, currentData)
          : field.options;
    }
    return (
      <Grid item xs={12} sm={6} key={field.key}>
        {fieldType === "select" || fieldType === "multiselect" ? (
          <FormControl fullWidth>
            <InputLabel>{field.label}</InputLabel>
            <Select
              value={
                formData[field.key] ||
                (fieldType === "multiselect" ? [] : "")
              }
              label={field.label}
              onChange={(e) => handleFieldChange(field.key, e.target.value)}
              multiple={fieldType === "multiselect"}
            >
              {options.map((opt) => (
                <MenuItem key={opt} value={opt}>
                  {opt}
                </MenuItem>
              ))}
            </Select>
          </FormControl>
        ) : (
          <TextField
            fullWidth
            label={field.label}
            value={formData[field.key] || ""}
            onChange={(e) => handleFieldChange(field.key, e.target.value)}
          />
        )}
      </Grid>
    );
  };

  const isApplyEnabled = () => {
    // Basic validation: must have an operation selected and a config loaded.
    if (!selectedOperation || !currentConfig) return false;
    // You may add further field-specific validations here.
    return true;
  };

  const handleApply = async () => {
    if (!currentConfig) return;
    try {
      const result = await ExplorerService.handleOperation({
        userId,
        operation_type: selectedCategory, // serves as the expected return type
        operation_name: selectedOperation,
        operation_params: formData,
      });
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
        {/* Operation Category Selection */}
        <Grid item xs={12} sm={4}>
          <FormControl fullWidth>
            <InputLabel>Operation Category</InputLabel>
            <Select
              value={selectedCategory}
              label="Operation Category"
              onChange={(e) => {
                setSelectedCategory(e.target.value);
                setSelectedOperation("");
                setFormData({});
              }}
            >
              {Object.keys(operationCategories).map((cat) => (
                <MenuItem key={cat} value={cat}>
                  {operationCategories[cat].label}
                </MenuItem>
              ))}
            </Select>
          </FormControl>
        </Grid>
        {/* Operation Name Selection */}
        <Grid item xs={12} sm={4}>
          <FormControl fullWidth>
            <InputLabel>Operation</InputLabel>
            <Select
              value={selectedOperation}
              label="Operation"
              onChange={(e) => setSelectedOperation(e.target.value)}
            >
              {operationCategories[selectedCategory].operations.map((op) => (
                <MenuItem key={op} value={op}>
                  {op.charAt(0).toUpperCase() + op.slice(1)}
                </MenuItem>
              ))}
            </Select>
          </FormControl>
        </Grid>
        {/* Render dynamic fields for categories that have a config */}
        {(selectedCategory === "raw" ||
          selectedCategory === "metric" ||
          selectedCategory === "list" ||
          selectedCategory === "enriched") &&
          currentConfig &&
          currentConfig.fields.map(renderField)}
        <Grid item xs={12}>
          <Button
            variant="contained"
            color="primary"
            onClick={handleApply}
            disabled={!isApplyEnabled()}
          >
            Apply
          </Button>
        </Grid>
      </Grid>
    </Box>
  );
};

export default DataOperation;
