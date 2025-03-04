// EnrichedOperationsConfig.js

const EnrichedOperationsConfig = {
    group_aggregate: {
      operationName: "group_aggregate",
      fields: [
        {
          key: "group_columns",
          label: "Group Columns (Select up to 3)",
          type: "multiselect",
          // Instead of offering "fields" as an option, we combine a list of static columns
          // with the dynamic fields keys retrieved from the backend.
          // For dynamic fields, we transform each key so that its display label is
          // "key (fields)" but its value remains the key.
          options: (formData, currentData) => {
            // Static columns that are not JSON fields:
            const staticOptions = ["date", "functionalities", "subject matters", "general categories"];
            const staticOptionObjects = staticOptions.map(opt => ({ value: opt, label: opt }));
            // Dynamic fields keys from the "fields" column:
            const fieldOptions = (currentData.uniqueJsonKeys || []).map(key => ({
              value: key,
              label: `${key} (fields)`
            }));
            return [...staticOptionObjects, ...fieldOptions];
          },
        },
        {
          key: "frequency",
          label: "Date Grouping Frequency",
          type: "select",
          options: ["daily", "weekly", "monthly", "yearly"],
          // Only show if "date" is among the selected group columns
          displayCondition: (formData) =>
            formData.group_columns && formData.group_columns.includes("date"),
        },
        {
          key: "aggregate_operation",
          label: "Aggregation Operation",
          type: "select",
          options: ["average", "count", "sum", "min", "max"],
        },
        {
          key: "target_column",
          label: "Target Column for Aggregation",
          type: "select",
          // We do a similar transformation here: allow static "date" plus dynamic JSON key options
          // where the dynamic ones display with a "(fields)" postfix.
          options: (formData, currentData) => {
            const staticOptions = ["date"];
            const staticOptionObjects = staticOptions.map(opt => ({ value: opt, label: opt }));
            const fieldOptions = (currentData.uniqueJsonKeys || []).map(key => ({
              value: key,
              label: `${key} (fields)`
            }));
            return [...staticOptionObjects, ...fieldOptions];
          },
        },
      ],
    },
  };
  
  export default EnrichedOperationsConfig;
  