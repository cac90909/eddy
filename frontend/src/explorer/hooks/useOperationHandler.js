import ExplorerService from '../services/ExplorerService';
import { OPERATION_CONFIG } from '../config/operation_config';

export function useOperationHandler(userId) {
  return async function handleApplyOperation(operationName, operationArguments = {}) {
    const operationConfig = OPERATION_CONFIG[operationName];
    if (!operationConfig) {
      throw new Error(`Unknown operation: ${operationName}`);
    }

    const httpMethod = operationConfig.httpMethod;

    try {
      const response = await ExplorerService.callOperation({
        operationName,
        operationArguments,
        userId,
        httpMethod
      });

      return {
        data: response.data,
        meta: response.meta
      };
    } catch (err) {
      console.error("Operation failed:", err);
      throw err;
    }
  };
}
