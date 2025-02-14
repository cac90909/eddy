import axios from "axios";

const EXPLORER_BASE_URL = "http://localhost:8000/explorer";

const ExplorerService = {
  /**
   * Generic handler that sends a request to the explorer endpoint using the specified HTTP method.
   *
   * For GET requests, parameters are sent as query parameters.
   * For POST, PUT, DELETE, parameters are sent in the request body as JSON.
   *
   * @param {number|string} userId 
   * @param {string} operationType 
   * @param {object} operationParams 
   * @param {string} method - HTTP method ("GET", "POST", "PUT", "DELETE")
   * @returns {Promise<any>} - The response data.
   */
  handleOperation: async (userId, operationType, operationParams = {}, method = "GET") => {
    console.log("ExplorerService.handleOperation called with:", { userId, operationType, method, operationParams });
    method = method.toUpperCase();
    // Ensure operationParams is always an object.
    const opParams = JSON.stringify(operationParams || {});
    let response;
    const config = {
      headers: {
        "Content-Type": "application/json"
      }
    };
    if (method === "GET") {
      response = await axios.get(`${EXPLORER_BASE_URL}/`, {
        params: {
          user_id: userId,
          operation_type: operationType,
          operation_params: opParams,
        },
      });
    } else if (method === "POST") {
      response = await axios.post(`${EXPLORER_BASE_URL}/`, {
        user_id: userId,
        operation_type: operationType,
        operation_params: opParams,
      }, config);
    } else if (method === "PUT") {
      response = await axios.put(`${EXPLORER_BASE_URL}/`, {
        user_id: userId,
        operation_type: operationType,
        operation_params: opParams,
      }, config);
    } else if (method === "DELETE") {
      response = await axios.delete(`${EXPLORER_BASE_URL}/`, {
        data: {
          user_id: userId,
          operation_type: operationType,
          operation_params: opParams,
        },
        ...config
      });
    }
    console.log("ExplorerService.handleOperation response:", response.data);
    return response.data;
  },

  initUser: async (userId) => {
    console.log("ExplorerService.initUser called with userId:", userId);
    return await ExplorerService.handleOperation(userId, "init_user", {});
  },

  filterData: async (userId, columnName, filterValue, filterType) => {
    console.log("ExplorerService.filterData called with:", { userId, columnName, filterValue, filterType });
    const params = { column_name: columnName, filter_value: filterValue, filter_type: filterType };
    return await ExplorerService.handleOperation(userId, "filter", params);
  },

  traverseData: async (userId, startId, traversal_directions) => {
    console.log("ExplorerService.traverseData called with:", { userId, startId, traversal_directions });
    const params = { start_id: startId, traversal_directions: traversal_directions };
    return await ExplorerService.handleOperation(userId, "traverse", params);
  },

  // Use "getUniqueColumnValues" as the operation type.
  getUniqueColumnValues: async (userId, columnName) => {
    console.log("ExplorerService.getUniqueColumnValues called with:", { userId, columnName });
    const params = { column_name: columnName };
    return await ExplorerService.handleOperation(userId, "get_unique_column_values", params);
  },

  getUniqueJsonKeys: async (userId) => {
    console.log("ExplorerService.getUniqueJsonKeys called with userId:", userId);
    return await ExplorerService.handleOperation(userId, "get_unique_json_keys", {});
  },

  getUniqueJsonValues: async (userId) => {
    console.log("ExplorerService.getUniqueJsonValues called with userId:", userId);
    return await ExplorerService.handleOperation(userId, "get_unique_json_values", {});
  },

  undoOperation: async (userId) => {
    console.log("ExplorerService.undoOperation called with userId:", userId);
    return await ExplorerService.handleOperation(userId, "undo_operation", {}, "DELETE");
  },

  endSession: async (userId) => {
    console.log("ExplorerService.endSession called with userId:", userId);
    return await ExplorerService.handleOperation(userId, "end_explorer_session", {}, "DELETE");
  },

  deleteSnapshot: async (userId, snapshotId) => {
    console.log("ExplorerService.deleteSnapshot called with:", { userId, snapshotId });
    const params = { snapshot_id: snapshotId };
    return await ExplorerService.handleOperation(userId, "delete_snapshot", params, "DELETE");
  },

  updateSnapshot: async (userId, snapshotId, title, description) => {
    console.log("ExplorerService.updateSnapshot called with:", { userId, snapshotId, title, description });
    const params = { snapshot_id: snapshotId, title, description };
    return await ExplorerService.handleOperation(userId, "update_snapshot", params, "PUT");
  },

  saveSnapshot: async (userId, title, description) => {
    console.log("ExplorerService.saveSnapshot called with:", { userId, title, description });
    const params = { title, description };
    return await ExplorerService.handleOperation(userId, "save_snapshot", params, "POST");
  },

  loadSnapshot: async (userId, snapshotId) => {
    console.log("ExplorerService.loadSnapshot called with:", { userId, snapshotId });
    const params = { snapshot_id: snapshotId };
    return await ExplorerService.handleOperation(userId, "load_snapshot", params);
  },

  getAllSnapshots: async (userId) => {
    console.log("ExplorerService.getAllSnapshots called with userId:", userId);
    return await ExplorerService.handleOperation(userId, "get_all_snapshots", {});
  },
};

export default ExplorerService;