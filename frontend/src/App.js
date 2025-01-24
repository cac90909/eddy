import React from "react";
import { BrowserRouter as Router, Routes, Route } from "react-router-dom";
import LandingPage from "./pages/LandingPage";
import ExplorerPage from "./explorer/pages/ExplorerPage";

function App() {
  return (
    <Router>
      <Routes>
        <Route path="/" element={<LandingPage />} />
        <Route path="/explorer" element={<ExplorerPage />} />
      </Routes>
    </Router>
  );
}

export default App;
