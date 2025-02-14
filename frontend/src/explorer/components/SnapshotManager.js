import React, { useState, useEffect } from "react";
import {
  Button,
  Modal,
  Box,
  Typography,
  TextField,
  List,
  ListItem,
  ListItemText,
  Divider,
  Stack,
} from "@mui/material";
import ExplorerService from "../services/ExplorerService";

const modalStyle = {
  position: "absolute",
  top: "50%",
  left: "50%",
  transform: "translate(-50%, -50%)",
  width: 400,
  bgcolor: "background.paper",
  p: 4,
  boxShadow: 24,
};

const SnapshotManager = ({ userId, onSnapshotLoad, onReset }) => {
  const [openSave, setOpenSave] = useState(false);
  const [snapshotTitle, setSnapshotTitle] = useState("");
  const [snapshotDescription, setSnapshotDescription] = useState("");
  const [openLoad, setOpenLoad] = useState(false);
  const [snapshots, setSnapshots] = useState([]);

  useEffect(() => {
    if (openLoad) {
      ExplorerService.getAllSnapshots(userId)
        .then((data) => setSnapshots(data))
        .catch((err) => console.error("Error fetching snapshots:", err));
    }
  }, [openLoad, userId]);

  const handleSave = async () => {
    try {
      const result = await ExplorerService.saveSnapshot(userId, snapshotTitle, snapshotDescription);
      setOpenSave(false);
      setSnapshotTitle("");
      setSnapshotDescription("");
      console.log("Snapshot saved:", result);
      alert("Snapshot saved successfully!");
    } catch (error) {
      console.error("Error saving snapshot:", error);
      alert("Error saving snapshot.");
    }
  };

  const handleLoad = async (snapshotId) => {
    try {
      const loadedData = await ExplorerService.loadSnapshot(userId, snapshotId);
      onSnapshotLoad(loadedData);
      setOpenLoad(false);
    } catch (error) {
      console.error("Error loading snapshot:", error);
      alert("Error loading snapshot.");
    }
  };

  const handleDelete = async (snapshotId) => {
    try {
      const result = await ExplorerService.deleteSnapshot(userId, snapshotId);
      console.log("Snapshot deleted:", result);
      // Optionally refresh snapshots list.
    } catch (error) {
      console.error("Error deleting snapshot:", error);
      alert("Error deleting snapshot.");
    }
  };

  const handleUpdate = async (snapshotId, title, description) => {
    try {
      const result = await ExplorerService.updateSnapshot(userId, snapshotId, title, description);
      console.log("Snapshot updated:", result);
    } catch (error) {
      console.error("Error updating snapshot:", error);
      alert("Error updating snapshot.");
    }
  };

  const handleUndo = async () => {
    try {
      const previousData = await ExplorerService.undoOperation(userId);
      onSnapshotLoad(previousData);
    } catch (error) {
      console.error("Error undoing operation:", error);
      alert("Error undoing operation.");
    }
  };

  const handleReset = async () => {
    try {
      const initData = await ExplorerService.initUser(userId);
      onReset(initData);
    } catch (error) {
      console.error("Error resetting snapshot data:", error);
      alert("Error resetting snapshot data.");
    }
  };

  return (
    <Stack direction="row" spacing={1} alignItems="center">
      <Button variant="outlined" color="secondary" onClick={handleReset}>
        Reset
      </Button>
      <Button variant="outlined" color="secondary" onClick={handleUndo}>
        Undo
      </Button>
      <Button variant="contained" color="secondary" onClick={() => setOpenSave(true)}>
        Save
      </Button>
      <Button variant="outlined" color="secondary" onClick={() => setOpenLoad(true)}>
        Load
      </Button>
      <Modal open={openSave} onClose={() => setOpenSave(false)}>
        <Box sx={modalStyle}>
          <Typography variant="h6" gutterBottom>
            Save Snapshot
          </Typography>
          <TextField
            label="Title"
            fullWidth
            value={snapshotTitle}
            onChange={(e) => setSnapshotTitle(e.target.value)}
            margin="normal"
          />
          <TextField
            label="Description"
            fullWidth
            multiline
            rows={3}
            value={snapshotDescription}
            onChange={(e) => setSnapshotDescription(e.target.value)}
            margin="normal"
          />
          <Box sx={{ mt: 2, textAlign: "right" }}>
            <Button onClick={() => setOpenSave(false)} sx={{ mr: 1 }}>
              Cancel
            </Button>
            <Button variant="contained" color="primary" onClick={handleSave}>
              Save
            </Button>
          </Box>
        </Box>
      </Modal>
      <Modal open={openLoad} onClose={() => setOpenLoad(false)}>
        <Box sx={modalStyle}>
          <Typography variant="h6" gutterBottom>
            Load Snapshot
          </Typography>
          <Divider />
          {snapshots.length > 0 ? (
            <List>
              {snapshots.map((snapshot) => (
                <ListItem button key={snapshot.id} onClick={() => handleLoad(snapshot.snapshot_id)}>
                  <ListItemText primary={snapshot.title} secondary={snapshot.description} />
                </ListItem>
              ))}
            </List>
          ) : (
            <Typography variant="body1" sx={{ mt: 2 }}>
              No snapshots available.
            </Typography>
          )}
          <Box sx={{ mt: 2, textAlign: "right" }}>
            <Button onClick={() => setOpenLoad(false)}>Close</Button>
          </Box>
        </Box>
      </Modal>
    </Stack>
  );
};

export default SnapshotManager;