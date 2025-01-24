import React, { useState } from "react";
import { useNavigate } from "react-router-dom";
import { TextField, Button, Box, Typography } from "@mui/material";

function LandingPage() {
  const [userId, setUserId] = useState("");
  const [error, setError] = useState("");
  const navigate = useNavigate();

  const handleNavigate = (path) => {
    if (!userId.trim()) {
      setError("User ID is required.");
      return;
    }
    setError(""); // Clear any previous errors
    navigate(path, { state: { userId } }); // Pass the userId via state
  };

  return (
    <Box
      display="flex"
      flexDirection="column"
      justifyContent="center"
      alignItems="center"
      height="100vh"
      gap="1rem"
    >
      <Typography variant="h4" component="h1">
        Welcome to the App
      </Typography>

      <TextField
        label="Enter User ID"
        value={userId}
        onChange={(e) => setUserId(e.target.value)}
        variant="outlined"
        error={!!error}
        helperText={error}
        style={{ width: "300px" }}
      />

      <Box display="flex" flexDirection="column" alignItems="center" gap="1rem">
        <Button
          variant="contained"
          color="primary"
          onClick={() => handleNavigate("/explorer")}
          style={{ width: "200px" }}
        >
          Explorer
        </Button>

        <Button
          variant="contained"
          color="secondary"
          disabled
          style={{ width: "200px" }}
        >
          Metrics (Coming Soon)
        </Button>

        <Button
          variant="contained"
          color="secondary"
          disabled
          style={{ width: "200px" }}
        >
          Dashboard (Coming Soon)
        </Button>
      </Box>
    </Box>
  );
}

export default LandingPage;
