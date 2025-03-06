import axios from "axios";

const EXPLORER_BASE_URL = "http://localhost:8000/explorer";

const ExplorerService = {
  /**
   * Generic handler that sends a request to the explorer endpoint.
   *
   * This function now accepts an object with:
   * - userId: the user id
   * - operation_name: the specific operation (e.g., "filter", "traverse", "init", etc.)
   * - operation_arguments: the parameters for the operation (default is an empty object)
   * - method: HTTP method ("GET", "POST", "PUT", "DELETE"; default is "GET")
   *
   * For GET and DELETE, parameters are sent as query parameters.
   * For POST and PUT, parameters are sent in the request body as JSON.
   */
  handleOperation: async ({
    userId,
    operation_name,
    operation_arguments = {},
    method = "GET",
  }) => {
    console.log("ExplorerService.handleOperation called with:", {
      userId,
      operation_name,
      method,
      operation_arguments,
    });
    method = method.toUpperCase();
    const opArgs = JSON.stringify(operation_arguments || {});
    const config = {
      headers: {
        "Content-Type": "application/json",
      },
    };
    let response;
    console.log(userId, operation_name, opArgs);
    if (method === "GET") {
      response = await axios.get(`${EXPLORER_BASE_URL}/`, {
        params: {
          user_id: userId,
          operation_name: operation_name,
          operation_arguments: opArgs,
        }
      });
    } else if (method === "POST") {
      response = await axios.post(
        `${EXPLORER_BASE_URL}/`,
        {
          user_id: userId,
          operation_name: operation_name,
          operation_arguments: opArgs,
        },
        config
      );
    } else if (method === "PUT") {
      response = await axios.put(
        `${EXPLORER_BASE_URL}/`,
        {
          user_id: userId,
          operation_name: operation_name,
          operation_arguments: opArgs,
        },
        config
      );
    } else if (method === "DELETE") {
      // DELETE now passes parameters via query opArgs
      response = await axios.delete(`${EXPLORER_BASE_URL}/`, {
        params: {
          user_id: userId,
          operation_name: operation_name,
          operation_arguments: opArgs,
        },
        ...config,
      });
    }
    const responseData = response && response.data ? response.data : {};
    console.log("ExplorerService.handleOperation response:", responseData);
    return responseData;
  },

  // Raw operations
  initUser: async (userId) => {
    console.log("ExplorerService.initUser called with userId:", userId);
    return await ExplorerService.handleOperation({
      userId: userId,
      operation_name: "init_user",
      operation_arguments: {},
    });
  },

  reset: async (userId) => {
    console.log("ExplorerService.initUser called with userId:", userId);
    return await ExplorerService.handleOperation({
      userId: userId,
      operation_name: "reset",
      operation_arguments: {},
    });
  },

  filterData: async (userId, columnName, filterValue, filterType) => {
    console.log("ExplorerService.filterData called with:", {
      userId,
      columnName,
      filterValue,
      filterType,
    });
    const opArgs = { column_name: columnName, filter_value: filterValue, filter_type: filterType };
    return await ExplorerService.handleOperation({
      userId: userId, 
      operation_name: "filter",
      operation_arguments: opArgs,
    });
  },

  traverseData: async (userId, startId, traversalDirections) => {
    console.log("ExplorerService.traverseData called with:", { userId, startId, traversalDirections });
    const opArgs = { start_id: startId, traversal_directions: traversalDirections };
    return await ExplorerService.handleOperation({
      userId: userId, 
      operation_name: "traverse",
      operation_arguments: opArgs,
    });
  },

  // List operations
  getUniqueColumnValues: async (userId, columnName) => {
    console.log("ExplorerService.getUniqueColumnValues called with:", { userId, columnName });
    const opArgs = { column_name: columnName };
    return await ExplorerService.handleOperation({
      userId: userId,
      operation_name: "get_unique_column_values",
      operation_arguments: opArgs,
    });
  },

  getUniqueJsonKeys: async (userId) => {
    console.log("ExplorerService.getUniqueJsonKeys called with:", { userId });
    const opArgs = {};
    return await ExplorerService.handleOperation({
      userId: userId, 
      operation_name: "get_unique_json_keys",
      operation_arguments: opArgs,
    });
  },

  getUniqueJsonKeyValues: async (userId) => {
    console.log("ExplorerService.getUniqueJsonValues called with:", { userId });
    const opArgs = {};
    return await ExplorerService.handleOperation({
      userId: userId, 
      operation_name: "get_unique_json_key_values",
      operation_arguments: opArgs,
    });
  },

  getUniqueJsonValues: async (userId) => {
    console.log("ExplorerService.getUniqueJsonValues called with:", { userId });
    const opArgs = {};
    return await ExplorerService.handleOperation({
      userId: userId, 
      operation_name: "get_unique_json_values",
      operation_arguments: opArgs,
    });
  },

  getUniqueColumnValuesFilterOptions: async (userId, columnName) => {
    console.log("ExplorerService.getUniqueColumnValuesFilterOptions called with:", { userId, columnName });
    const opArgs = { column_name: columnName };
    return await ExplorerService.handleOperation({
      userId: userId,
      operation_name: "get_unique_column_values_filter_options",
      operation_arguments: opArgs,
    });
  },

  getUniqueJsonKeyFilterOptionss: async (userId) => {
    console.log("ExplorerService.getUniqueJsonKeysFilterOptions called with:", { userId });
    const opArgs = {};
    return await ExplorerService.handleOperation({
      userId: userId, 
      operation_name: "get_unique_json_keys_filter_options",
      operation_arguments: opArgs,
    });
  },

  getUniqueJsonKeyValuesFilterOptions: async (userId) => {
    console.log("ExplorerService.getUniqueJsonValuesFilterOptions called with:", { userId });
    const opArgs = {};
    return await ExplorerService.handleOperation({
      userId: userId, 
      operation_name: "get_unique_json_key_values_filter_options",
      operation_arguments: opArgs,
    });
  },

  getUniqueJsonValuesFilterOptions: async (userId) => {
    console.log("ExplorerService.getUniqueJsonValuesFilterOptions called with:", { userId });
    const opArgs = {};
    return await ExplorerService.handleOperation({
      userId: userId, 
      operation_name: "get_unique_json_values_filter_options",
      operation_arguments: opArgs,
    });
  },

  // State operations
  undoOperation: async (userId) => {
    console.log("ExplorerService.undoOperation called with userId:", userId);
    return await ExplorerService.handleOperation({
      userId: userId, 
      operation_name: "undo_operation",
      operation_arguments: {},
      method: "DELETE",
    });
  },

  endSession: async (userId) => {
    console.log("ExplorerService.endSession called with userId:", userId);
    return await ExplorerService.handleOperation({
      userId: userId, 
      operation_name: "end_explorer_session",
      operation_arguments: {},
      method: "DELETE",
    });
  },
  getOperationChain: async (userId) => {
    console.log("ExplorerService.getOperationChain called with userId:", userId);
    return await ExplorerService.handleOperation({
      userId, 
      operation_name: "get_operation_chain",
      operation_arguments: {},
    });
  },
  getOperationChainOperations: async (userId) => {
    console.log("ExplorerService.getOperationChainOperations called with userId:", userId);
    return await ExplorerService.handleOperation({
      userId,
      operation_name: "get_operation_chain_operations",
      operation_arguments: {},
    });
  },
  getOperationChainResults: async (userId) => {
    console.log("ExplorerService.getOperationChainOperations called with userId:", userId);
    return await ExplorerService.handleOperation({
      userId,
      operation_name: "get_operation_chain_results",
      operation_arguments: {},
    });
  },

  // Snapshot operations (type "snapshot")
  deleteSnapshot: async (userId, snapshotId) => {
    console.log("ExplorerService.deleteSnapshot called with:", { userId, snapshotId });
    const opArgs = { snapshot_id: snapshotId };
    return await ExplorerService.handleOperation({
      userId: userId, 
      operation_name: "delete_snapshot",
      operation_arguments: opArgs,
      method: "DELETE",
    });
  },

  updateSnapshot: async (userId, snapshotId, title, description) => {
    console.log("ExplorerService.updateSnapshot called with:", { userId, snapshotId, title, description });
    const opArgs = { snapshot_id: snapshotId, title, description };
    return await ExplorerService.handleOperation({
      userId: userId, 
      operation_name: "update_snapshot",
      operation_arguments: opArgs,
      method: "PUT",
    });
  },

  saveSnapshot: async (userId, title, description) => {
    console.log("ExplorerService.saveSnapshot called with:", { userId, title, description });
    const opArgs = { title, description };
    return await ExplorerService.handleOperation({
      userId: userId,
      operation_name: "save_snapshot",
      operation_arguments: opArgs,
      method: "POST",
    });
  },

  loadSnapshot: async (userId, snapshotId) => {
    console.log("ExplorerService.loadSnapshot called with:", { userId, snapshotId });
    const opArgs = { snapshot_id: snapshotId };
    return await ExplorerService.handleOperation({
      userId: userId, 
      operation_name: "load_snapshot",
      operation_arguments: opArgs,
    });
  },

  getAllSnapshots: async (userId) => {
    console.log("ExplorerService.getAllSnapshots called with userId:", userId);
    return await ExplorerService.handleOperation({
      userId: userId, 
      operation_name: "get_all_snapshots",
      operation_arguments: {},
    });
  },

  // Metric operations
  getCount: async (userId, columnName) => {
    console.log("ExplorerService.getCount called with:", { userId, columnName });
    const opArgs = { column_name: columnName };
    return await ExplorerService.handleOperation({
      userId: userId, 
      operation_name: "get_count",
      operation_arguments: opArgs,
    });
  },

  getAverage: async (userId, columnName) => {
    console.log("ExplorerService.getAverage called with:", { userId, columnName });
    const opArgs = { column_name: columnName };
    return await ExplorerService.handleOperation({
      userId: userId, 
      operation_name: "get_average",
      operation_arguments: opArgs,
    });
  },

  getSum: async (userId, columnName) => {
    console.log("ExplorerService.getSum called with:", { userId, columnName });
    const opArgs = { column_name: columnName };
    return await ExplorerService.handleOperation({
      userId: userId, 
      operation_name: "get_sum",
      operation_arguments: opArgs,
    });
  },

  getMax: async (userId, columnName) => {
    console.log("ExplorerService.getMax called with:", { userId, columnName });
    const opArgs = { column_name: columnName };
    return await ExplorerService.handleOperation({
      userId: userId, 
      operation_name: "get_max",
      operation_arguments: opArgs,
    });
  },

  getMin: async (userId, columnName) => {
    console.log("ExplorerService.getMin called with:", { userId, columnName });
    const opArgs = { column_name: columnName };
    return await ExplorerService.handleOperation({
      userId: userId, 
      operation_name: "get_min",
      operation_arguments: opArgs,
    });
  },

  //Enrichment Operations
  groupAggregate: async (userId, opArgs) => {
    console.log("ExplorerService.groupAggregate called with:", { userId, opArgs });
    return await ExplorerService.handleOperation({
      userId, 
      operation_name: "group_aggregate",
      operation_arguments: opArgs,
    });
  },

};





export default ExplorerService;
