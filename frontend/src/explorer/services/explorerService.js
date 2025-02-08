// src/services/ExplorerService.js
import axios from "axios";

const EXPLORER_BASE_URL = "http://localhost:8000/explorer";

const ExplorerService = {
  handleOperation: async (userId, operationType, operationParams = {}) => {
    console.log("ExplorerService.handleOperation called with:", { userId, operationType, operationParams });
    // If operationParams is an object, we send its JSON string
    const response = await axios.get(`${EXPLORER_BASE_URL}/`, {
      params: {
        user_id: userId,
        operation_type: operationType,
        operation_params: JSON.stringify(operationParams)
      }
    });
    console.log("ExplorerService.handleOperation response:", response.data);
    return response.data;
  },

  // Convenience methods:
  initUser: async (userId) => {
    console.log("ExplorerService.initUser called with userId:", userId);
    return await ExplorerService.handleOperation(userId, "init_user");
  },

  filterData: async (userId, columnName, filterValue, filterType) => {
    console.log("ExplorerService.filterData called with:", { userId, columnName, filterValue, filterType });
    const params = { column_name: columnName, filter_value: filterValue, filter_type: filterType };
    return await ExplorerService.handleOperation(userId, "filter", params);
  },

  traverseData: async (userId, startId, columnName) => {
    console.log("ExplorerService.traverseData called with:", { userId, startId, columnName });
    const params = { start_id: startId, column_name: columnName };
    return await ExplorerService.handleOperation(userId, "traverse", params);
  },

  // Additional operations can be defined similarly:
  getUniqueFilterOptions: async (userId, columnName) => {
    console.log("ExplorerService.getUniqueFilterOptions called with:", { userId, columnName });
    const params = { column_name: columnName };
    return await ExplorerService.handleOperation(userId, "get_filter_values", params);
  },

  getJsonKeys: async (userId) => {
    console.log("ExplorerService.getJsonKeys called with userId:", userId);
    return await ExplorerService.handleOperation(userId, "get_json_keys");
  },

  getJsonValues: async (userId) => {
    console.log("ExplorerService.getJsonValues called with userId:", userId);
    return await ExplorerService.handleOperation(userId, "get_json_values");
  },

  undoOperation: async (userId) => {
    console.log("ExplorerService.undoOperation called with userId:", userId);
    return await ExplorerService.handleOperation(userId, "undo_operation");
  },

  saveSnapshot: async (userId, title, description) => {
    console.log("ExplorerService.saveSnapshot called with:", { userId, title, description });
    const params = { title, description };
    return await ExplorerService.handleOperation(userId, "save_snapshot", params);
  },

  loadSnapshot: async (userId, snapshotId) => {
    console.log("ExplorerService.loadSnapshot called with:", { userId, snapshotId });
    const params = { snapshot_id: snapshotId };
    return await ExplorerService.handleOperation(userId, "load_snapshot", params);
  },

  deleteSnapshot: async (userId, snapshotId) => {
    console.log("ExplorerService.deleteSnapshot called with:", { userId, snapshotId });
    const params = { snapshot_id: snapshotId };
    return await ExplorerService.handleOperation(userId, "delete_snapshot", params);
  },

  updateSnapshot: async (userId, snapshotId, title, description) => {
    console.log("ExplorerService.updateSnapshot called with:", { userId, snapshotId, title, description });
    const params = { snapshot_id: snapshotId, title, description };
    return await ExplorerService.handleOperation(userId, "update_snapshot", params);
  },

  getAllSnapshots: async (userId) => {
    console.log("ExplorerService.getAllSnapshots called with userId:", userId);
    return await ExplorerService.handleOperation(userId, "get_all_snapshots");
  },

  endSession: async (userId) => {
    console.log("ExplorerService.endSession called with userId:", userId);
    return await ExplorerService.handleOperation(userId, "end_explorer_session");
  }
};

export default ExplorerService;
