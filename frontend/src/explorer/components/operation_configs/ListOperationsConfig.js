// ListOperationsConfig.js

const ListOperationsConfig = {
    unique: {
      operationName: "unique",
      fields: [
        {
          key: "column",
          label: "Column",
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
          key: "uniqueType",
          label: "Unique Type",
          type: "select",
          options: () => ["keys", "values"],
          displayCondition: (formData) => formData.column === "fields",
        },
      ],
    },
  };
  
  export default ListOperationsConfig;
  