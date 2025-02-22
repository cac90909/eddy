// RawOperationsConfig.js
const RawOperationsConfig = {
    filter: {
      operationName: "filter",
      fields: [
        {
          key: "column",
          label: "Filter Column",
          type: "select",
          options: [
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
          ],
        },
        {
          key: "jsonKey",
          label: "JSON Key",
          type: "select",
          options: (formData, currentData) =>
            formData.column === "fields" ? currentData.uniqueJsonKeys || [] : [],
          displayCondition: (formData) => formData.column === "fields",
        },
        {
          key: "operator",
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
              "subject matters": ["array_contains", "array_not_contains"],
              "general categories": ["array_contains", "array_not_contains"],
              tags: ["array_contains", "array_not_contains"],
              parents_ids: ["array_contains", "array_not_contains"],
              children_ids: ["array_contains", "array_not_contains"],
              siblings_ids: ["array_contains", "array_not_contains"],
              fields: ["string_contains", "=", "!="],
            };
            return mapping[formData.column] || ["=", "!="];
          },
        },
        {
          key: "value",
          label: "Filter Value",
          type: (formData) => {
            const categoricalColumns = [
              "functionalities",
              "subject matters",
              "general categories",
              "tags",
              "parents_ids",
              "children_ids",
              "siblings_ids",
            ];
            return categoricalColumns.includes(formData.column) ? "select" : "input";
          },
          options: (formData, currentData) => {
            const categoricalColumns = [
              "functionalities",
              "subject matters",
              "general categories",
              "tags",
              "parents_ids",
              "children_ids",
              "siblings_ids",
            ];
            return categoricalColumns.includes(formData.column)
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
          key: "startId",
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
          key: "column",
          label: "Sort Column",
          type: "select",
          options: [
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
          ],
        },
        {
          key: "jsonKey",
          label: "JSON Key",
          type: "select",
          options: (formData, currentData) =>
            formData.column === "fields" ? currentData.uniqueJsonKeys || [] : [],
          displayCondition: (formData) => formData.column === "fields",
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
  