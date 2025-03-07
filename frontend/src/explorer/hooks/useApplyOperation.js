// hooks/useApplyOperation.js
import { useState } from "react";
import ExplorerService from "../services/ExplorerService";

export function useApplyOperation(userId) {
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);

  const applyOperation = async (operationName, operationParams) => {
    setLoading(true);
    setError(null);
    try {
      const result = await ExplorerService.handleOperation({
        userId,
        operation_name: operationName,
        operation_arguments: operationParams,
      });
      return result;
    } catch (err) {
      setError(err);
      throw err;
    } finally {
      setLoading(false);
    }
  };

  return { applyOperation, loading, error };
}
