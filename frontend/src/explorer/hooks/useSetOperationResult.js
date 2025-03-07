// hooks/useSessionData.js
import { useState } from "react";

const initialOperationResult = {
  data: [],
  dataType: null,
  dataOverview: null,
  operationsHistory: [],
  columns: [],
  resetKey: 0,
};

export function useSetOperationResult() {
  const [operationResult, setOperationResult] = useState(initialOperationResult);

  const updateOperationResult = (response) => {
    setOperationResult({
      data: response.data,
      dataType: response.meta.data_type,
      dataOverview: response.meta.data_overview,
      operationsHistory: response.meta.operations_history || [],
      columns: Object.keys(response.data[0] || {}),
      resetKey: sessionData.resetKey + 1,
    });
  };

  return { operationResult, updateOperationResult };
}
