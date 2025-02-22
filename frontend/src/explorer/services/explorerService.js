import axios from "axios";

const EXPLORER_BASE_URL = "http://localhost:8000/explorer";

const ExplorerService = {
  /**
   * Generic handler that sends a request to the explorer endpoint.
   *
   * This function now accepts an object with:
   * - userId: the user id
   * - operation_type: the expected return type (e.g., "explorer_raw", "explorer_metric", "explorer_enrichment", "explorer_list", "explorer_state", or "snapshot")
   * - operation_name: the specific operation (e.g., "filter", "traverse", "init", etc.)
   * - operation_params: the parameters for the operation (default is an empty object)
   * - method: HTTP method ("GET", "POST", "PUT", "DELETE"; default is "GET")
   *
   * For GET and DELETE, parameters are sent as query parameters.
   * For POST and PUT, parameters are sent in the request body as JSON.
   */
  handleOperation: async ({
    userId,
    operation_type,
    operation_name,
    operation_params = {},
    method = "GET",
  }) => {
    console.log("ExplorerService.handleOperation called with:", {
      userId,
      operation_type,
      operation_name,
      method,
      operation_params,
    });
    method = method.toUpperCase();
    const opParams = JSON.stringify(operation_params || {});
    const config = {
      headers: {
        "Content-Type": "application/json",
      },
    };
    let response;

    if (method === "GET") {
      response = await axios.get(`${EXPLORER_BASE_URL}/`, {
        params: {
          user_id: userId,
          operation_type: operation_type,
          operation_name: operation_name,
          operation_params: opParams,
        },
      });
    } else if (method === "POST") {
      response = await axios.post(
        `${EXPLORER_BASE_URL}/`,
        {
          user_id: userId,
          operation_type: operation_type,
          operation_name: operation_name,
          operation_params: opParams,
        },
        config
      );
    } else if (method === "PUT") {
      response = await axios.put(
        `${EXPLORER_BASE_URL}/`,
        {
          user_id: userId,
          operation_type: operation_type,
          operation_name: operation_name,
          operation_params: opParams,
        },
        config
      );
    } else if (method === "DELETE") {
      // DELETE now passes parameters via query params
      response = await axios.delete(`${EXPLORER_BASE_URL}/`, {
        params: {
          user_id: userId,
          operation_type: operation_type,
          operation_name: operation_name,
          operation_params: opParams,
        },
        ...config,
      });
    }
    console.log("ExplorerService.handleOperation response:", response.data);
    return response.data;
  },

  // Raw operations
  initUser: async (userId) => {
    console.log("ExplorerService.initUser called with userId:", userId);
    return await ExplorerService.handleOperation({
      userId: userId,
      operation_type: "explorer_raw",
      operation_name: "init",
      operation_params: {},
    });
  },

  filterData: async (userId, columnName, filterValue, filterType) => {
    console.log("ExplorerService.filterData called with:", {
      userId,
      columnName,
      filterValue,
      filterType,
    });
    const params = { column_name: columnName, filter_value: filterValue, filter_type: filterType };
    return await ExplorerService.handleOperation({
      userId: userId,
      operation_type: "explorer_raw",
      operation_name: "filter",
      operation_params: params,
    });
  },

  traverseData: async (userId, startId, traversalDirections) => {
    console.log("ExplorerService.traverseData called with:", { userId, startId, traversalDirections });
    const params = { start_id: startId, traversal_directions: traversalDirections };
    return await ExplorerService.handleOperation({
      userId: userId,
      operation_type: "explorer_raw",
      operation_name: "traverse",
      operation_params: params,
    });
  },

  // List operations
  getUniqueColumnValues: async (userId, columnName, filterOptionsCall = false) => {
    console.log("ExplorerService.getUniqueColumnValues called with:", { userId, columnName, filterOptionsCall });
    const params = { column_name: columnName, filter_options_call: filterOptionsCall };
    return await ExplorerService.handleOperation({
      userId: userId,
      operation_type: "explorer_list",
      operation_name: "get_unique_column_values",
      operation_params: params,
    });
  },

  getUniqueJsonKeys: async (userId, filterOptionsCall = false) => {
    console.log("ExplorerService.getUniqueJsonKeys called with:", { userId, filterOptionsCall });
    const params = { filter_options_call: filterOptionsCall };
    return await ExplorerService.handleOperation({
      userId: userId,
      operation_type: "explorer_list",
      operation_name: "get_unique_json_keys",
      operation_params: params,
    });
  },

  getUniqueJsonValues: async (userId, filterOptionsCall = false) => {
    console.log("ExplorerService.getUniqueJsonValues called with:", { userId, filterOptionsCall });
    const params = { filter_options_call: filterOptionsCall };
    return await ExplorerService.handleOperation({
      userId: userId,
      operation_type: "explorer_list",
      operation_name: "get_unique_json_values",
      operation_params: params,
    });
  },

  // State operations
  undoOperation: async (userId) => {
    console.log("ExplorerService.undoOperation called with userId:", userId);
    return await ExplorerService.handleOperation({
      userId: userId,
      operation_type: "explorer_raw",
      operation_name: "undo_operation",
      operation_params: {},
      method: "DELETE",
    });
  },

  endSession: async (userId) => {
    console.log("ExplorerService.endSession called with userId:", userId);
    return await ExplorerService.handleOperation({
      userId: userId,
      operation_type: "explorer_state",
      operation_name: "end_explorer_session",
      operation_params: {},
      method: "DELETE",
    });
  },
  getOperationChain: async (userId) => {
    console.log("ExplorerService.getOperationChain called with userId:", userId);
    return await ExplorerService.handleOperation({
      userId,
      operation_type: "explorer_state",
      operation_name: "get_operation_chain",
      operation_params: {},
    });
  },

  // Snapshot operations (type "snapshot")
  deleteSnapshot: async (userId, snapshotId) => {
    console.log("ExplorerService.deleteSnapshot called with:", { userId, snapshotId });
    const params = { snapshot_id: snapshotId };
    return await ExplorerService.handleOperation({
      userId: userId,
      operation_type: "snapshot",
      operation_name: "delete_snapshot",
      operation_params: params,
      method: "DELETE",
    });
  },

  updateSnapshot: async (userId, snapshotId, title, description) => {
    console.log("ExplorerService.updateSnapshot called with:", { userId, snapshotId, title, description });
    const params = { snapshot_id: snapshotId, title, description };
    return await ExplorerService.handleOperation({
      userId: userId,
      operation_type: "snapshot",
      operation_name: "update_snapshot",
      operation_params: params,
      method: "PUT",
    });
  },

  saveSnapshot: async (userId, title, description) => {
    console.log("ExplorerService.saveSnapshot called with:", { userId, title, description });
    const params = { title, description };
    return await ExplorerService.handleOperation({
      userId: userId,
      operation_type: "snapshot",
      operation_name: "save_snapshot",
      operation_params: params,
      method: "POST",
    });
  },

  loadSnapshot: async (userId, snapshotId) => {
    console.log("ExplorerService.loadSnapshot called with:", { userId, snapshotId });
    const params = { snapshot_id: snapshotId };
    return await ExplorerService.handleOperation({
      userId: userId,
      operation_type: "explorer_raw",
      operation_name: "load_snapshot",
      operation_params: params,
    });
  },

  getAllSnapshots: async (userId) => {
    console.log("ExplorerService.getAllSnapshots called with userId:", userId);
    return await ExplorerService.handleOperation({
      userId: userId,
      operation_type: "snapshot",
      operation_name: "get_all_snapshots",
      operation_params: {},
    });
  },

  // Metric operations
  getCount: async (userId, columnName) => {
    console.log("ExplorerService.getCount called with:", { userId, columnName });
    const params = { column_name: columnName };
    return await ExplorerService.handleOperation({
      userId: userId,
      operation_type: "explorer_metric",
      operation_name: "get_count",
      operation_params: params,
    });
  },

  getAverage: async (userId, columnName) => {
    console.log("ExplorerService.getAverage called with:", { userId, columnName });
    const params = { column_name: columnName };
    return await ExplorerService.handleOperation({
      userId: userId,
      operation_type: "explorer_metric",
      operation_name: "get_average",
      operation_params: params,
    });
  },

  getSum: async (userId, columnName) => {
    console.log("ExplorerService.getSum called with:", { userId, columnName });
    const params = { column_name: columnName };
    return await ExplorerService.handleOperation({
      userId: userId,
      operation_type: "explorer_metric",
      operation_name: "get_sum",
      operation_params: params,
    });
  },

  getMax: async (userId, columnName) => {
    console.log("ExplorerService.getMax called with:", { userId, columnName });
    const params = { column_name: columnName };
    return await ExplorerService.handleOperation({
      userId: userId,
      operation_type: "explorer_metric",
      operation_name: "get_max",
      operation_params: params,
    });
  },

  getMin: async (userId, columnName) => {
    console.log("ExplorerService.getMin called with:", { userId, columnName });
    const params = { column_name: columnName };
    return await ExplorerService.handleOperation({
      userId: userId,
      operation_type: "explorer_metric",
      operation_name: "get_min",
      operation_params: params,
    });
  },

  //Enrichment Operations
  groupAggregate: async (userId, params) => {
    console.log("ExplorerService.groupAggregate called with:", { userId, params });
    return await ExplorerService.handleOperation({
      userId,
      operation_type: "explorer_enrichment",
      operation_name: "group_aggregate",
      operation_params: params,
    });
  },

};





export default ExplorerService;
