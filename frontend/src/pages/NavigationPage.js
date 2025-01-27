import React from "react";
import { useNavigate } from "react-router-dom";
import { Button, Box, Typography } from "@mui/material";
import { useUser } from "../UserContext";

function NavigationPage() {
console.log("NavigationPage")
  const { userId } = useUser(); // Access userId from context
  const navigate = useNavigate();

  const handleNavigate = (path) => {
    navigate(path); // Navigate to the selected page
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
        Welcome, {userId}!
      </Typography>

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

export default NavigationPage;
