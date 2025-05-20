const RawOperationsConfig = {
  filter: {
    operationName: "filter",
    fields: [
      {
        key: "column_name", // was "filter_column"
        label: "Filter Column",
        type: "select",
        options: [
          "id",
          "entry_id",
          "date",
          "title",
          "text",
          "functionalities",
          "subject_matters",
          "general_categories",
          "tags",
          "parents_ids",
          "children_ids",
          "siblings_ids",
          "fields",
        ],
      },
      {
        key: "json_key", // was "jsonKey"
        label: "JSON Key",
        type: "select",
        options: (formData, currentData) =>
          formData.column_name === "fields" ? currentData.uniqueJsonKeys || [] : [],
        displayCondition: (formData) => formData.column_name === "fields",
      },
      {
        key: "filter_type",
        label: "Operator",
        type: "select",
        options: (formData) => {
          const mapping = {
            id: ["=", "!="],
            entry_id: ["=", "!="],
            date: [">", "<", "=", "!="],
            title: ["string_contains"],
            text: ["string_contains"],
            functionalities: ["array_contains", "array_not_contains"],
            subject_matters: ["array_contains", "array_not_contains"],
            general_categories: ["array_contains", "array_not_contains"],
            tags: ["array_contains", "array_not_contains"],
            parents_ids: ["array_contains", "array_not_contains"],
            children_ids: ["array_contains", "array_not_contains"],
            siblings_ids: ["array_contains", "array_not_contains"],
            fields: ["string_contains", "=", "!="],
          };
          return mapping[formData.column_name] || ["=", "!="];
        },
      },
      {
        key: "filter_value",
        label: "Filter Value",
        type: (formData) => {
          const categoricalColumns = [
            "functionalities",
            "subject_matters",
            "general_categories",
            "tags",
            "parents_ids",
            "children_ids",
            "siblings_ids",
          ];
          return categoricalColumns.includes(formData.column_name) ? "select" : "input";
        },
        options: (formData, currentData) => {
          const categoricalColumns = [
            "functionalities",
            "subject_matters",
            "general_categories",
            "tags",
            "parents_ids",
            "children_ids",
            "siblings_ids",
          ];
          return categoricalColumns.includes(formData.column_name)
            ? currentData.uniqueColumnValues || []
            : [];
        },
      },
    ],
  },
  traverse: {
    operationName: "traverse",
    fields: [
      {
        key: "start_id", // was "startId"
        label: "Start ID",
        type: "select",
        options: (formData, currentData) => currentData.uniqueEntryIds || [],
      },
      {
        key: "traversal_directions",
        label: "Traversal Directions",
        type: "multiselect",
        options: ["horizontal", "upwards", "downwards"],
      },
    ],
  },
  sort: {
    operationName: "sort",
    fields: [
      {
        key: "column_name", // was "column"
        label: "Sort Column",
        type: "select",
        options: [
          "id",
          "entry_id",
          "date",
          "title",
          "text",
          "functionalities",
          "subject_matters",
          "general_categories",
          "tags",
          "parents_ids",
          "children_ids",
          "siblings_ids",
          "fields",
        ],
      },
      {
        key: "json_key", // was "jsonKey"
        label: "JSON Key",
        type: "select",
        options: (formData, currentData) =>
          formData.column_name === "fields" ? currentData.uniqueJsonKeys || [] : [],
        displayCondition: (formData) => formData.column_name === "fields",
      },
      {
        key: "sortOrder",
        label: "Sort Order",
        type: "select",
        options: ["asc", "desc"],
      },
    ],
  },
  union: {
    operationName: "union",
    fields: [
      {
        key: "note",
        label: "Note",
        type: "input",
      },
    ],
  },
};

export default RawOperationsConfig;
