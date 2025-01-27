import React, { useState } from "react";
import { useNavigate } from "react-router-dom";
import { TextField, Button, Box, Typography } from "@mui/material";
import { useUser } from "../UserContext";

function SignInPage() {
console.log("SignInPage")
  const [inputValue, setInputValue] = useState("");
  const [error, setError] = useState("");
  const { setUserId } = useUser(); // Access UserContext
  const navigate = useNavigate();

  const handleSignIn = () => {
    if (!inputValue.trim()) {
      setError("User ID is required.");
      return;
    }
    setUserId(inputValue.trim()); // Set userId in context
    setError("");
    navigate("/navigation"); // Redirect to Navigation Page
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
        Sign In
      </Typography>

      <TextField
        label="Enter User ID"
        value={inputValue}
        onChange={(e) => setInputValue(e.target.value)}
        variant="outlined"
        error={!!error}
        helperText={error}
        style={{ width: "300px" }}
      />

      <Button
        variant="contained"
        onClick={handleSignIn}
        style={{ width: "200px" }}
      >
        Sign In
      </Button>
    </Box>
  );
}

export default SignInPage;
