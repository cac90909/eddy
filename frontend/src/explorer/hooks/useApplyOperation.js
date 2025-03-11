// hooks/useApplyOperation.js
import { useState } from "react";
import ExplorerService from "../services/ExplorerService";

export function useApplyOperation(userId) {
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);

  const applyOperation = async (operationName, operationArguments) => {
    setLoading(true);
    setError(null);
    try {
      const result = await ExplorerService.handleOperation({
        operation_name: operationName,
        operation_arguments: operationArguments,
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
