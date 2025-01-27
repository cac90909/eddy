import React from "react";
import { BrowserRouter as Router, Routes, Route } from "react-router-dom";
import { UserProvider } from "./UserContext";
import SignInPage from "./pages/SignInPage";
import NavigationPage from "./pages/NavigationPage";
import ExplorerPage from "./explorer/pages/ExplorerPage";

function App() {
console.log("App.js")
  return (
    <UserProvider>
      <Router>
        <Routes>
          <Route path="/" element={<SignInPage />} />
          <Route path="/navigation" element={<NavigationPage />} />
          <Route path="/explorer" element={<ExplorerPage />} />
        </Routes>
      </Router>
    </UserProvider>
  );
}

export default App;
