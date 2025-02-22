// MetricOperationsConfig.js
const MetricOperationsConfig = {
    getCount: {
      operationName: "getCount",
      fields: [
        {
          key: "column",
          label: "Column",
          type: "select",
          // Only allow "fields" for getCount
          options: () => ["id", "fields"],
        },
        {
          key: "subField",
          label: "JSON Sub Field",
          type: "select",
          options: (formData, currentData) =>
            formData.column === "fields" ? currentData.uniqueJsonKeys || [] : [],
          displayCondition: (formData) => formData.column === "fields",
        },
      ],
    },
    getAverage: {
      operationName: "getAverage",
      fields: [
        {
          key: "column",
          label: "Column",
          type: "select",
          // Only allow "fields"
          options: () => ["fields"],
        },
        {
          key: "subField",
          label: "JSON Sub Field",
          type: "select",
          options: (formData, currentData) =>
            formData.column === "fields" ? currentData.uniqueJsonKeys || [] : [],
          displayCondition: (formData) => formData.column === "fields",
        },
      ],
    },
    getSum: {
      operationName: "getSum",
      fields: [
        {
          key: "column",
          label: "Column",
          type: "select",
          // Only allow "fields"
          options: () => ["fields"],
        },
        {
          key: "subField",
          label: "JSON Sub Field",
          type: "select",
          options: (formData, currentData) =>
            formData.column === "fields" ? currentData.uniqueJsonKeys || [] : [],
          displayCondition: (formData) => formData.column === "fields",
        },
      ],
    },
    getMin: {
      operationName: "getMin",
      fields: [
        {
          key: "column",
          label: "Column",
          type: "select",
          // Allow "date" or "fields"
          options: () => ["date", "fields"],
        },
        {
          key: "subField",
          label: "JSON Sub Field",
          type: "select",
          options: (formData, currentData) =>
            formData.column === "fields" ? currentData.uniqueJsonKeys || [] : [],
          displayCondition: (formData) => formData.column === "fields",
        },
      ],
    },
    getMax: {
      operationName: "getMax",
      fields: [
        {
          key: "column",
          label: "Column",
          type: "select",
          // Allow "date" or "fields"
          options: () => ["date", "fields"],
        },
        {
          key: "subField",
          label: "JSON Sub Field",
          type: "select",
          options: (formData, currentData) =>
            formData.column === "fields" ? currentData.uniqueJsonKeys || [] : [],
          displayCondition: (formData) => formData.column === "fields",
        },
      ],
    },
  };
  
  export default MetricOperationsConfig;
  