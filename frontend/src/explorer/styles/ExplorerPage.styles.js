// ExplorerPage.styles.js
import { styled } from "@mui/system";
import Box from "@mui/material/Box";
import Paper from "@mui/material/Paper";
import Typography from "@mui/material/Typography";

export const ExplorerContainer = styled(Box)(({ theme }) => ({
  height: "100vh",
  padding: "1rem",
  flexDirection: "column",
  boxSizing: "border-box",
  overflow: "hidden",
}));

export const TopPanel = styled(Paper)(({ theme }) => ({
  flex: "0 0 10vh",
  display: "flex",
  justifyContent: "space-between",
  alignItems: "center",
  padding: "1rem",
  marginBottom: "1rem",
  height: "10%",
  backgroundColor: "#f5f5f5",
  borderRadius: "8px",
}));

export const MainContentContainer = styled(Box)(({ theme }) => ({
  display: "flex",
  height: "85%",
  gap: "1rem",
  flexDirection: "row",
}));

export const LeftPanel = styled(Paper)(({ theme }) => ({
  flex: 2,
  padding: "1rem",
  display: "flex",
  flexDirection: "column",
  height: "100%",
  backgroundColor: "#fafafa",
  borderRadius: "8px",
  overflow: "hidden",
}));

export const RightPanel = styled(Paper)(({ theme }) => ({
  flex: 3,
  padding: "1rem",
  display: "flex",
  flexDirection: "column",
  height: "100%",
  backgroundColor: "#fafafa",
  borderRadius: "8px",
  overflow: "hidden",
}));

export const DataDisplayContainer = styled(Box)(({ theme }) => ({
  flex: 7,
  overflow: "auto",
  marginBottom: "1rem",
}));

export const DataOverviewContainer = styled(Box)(({ theme }) => ({
  flex: 1,
  backgroundColor: "#e0e0e0",
  borderRadius: "4px",
  padding: "0.5rem",
  overflow: "auto",
}));
