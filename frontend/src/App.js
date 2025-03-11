import React, { useEffect } from "react";
import { BrowserRouter as Router, Routes, Route } from "react-router-dom";
import { UserSessionProvider } from ".contexts/UserSessionContext";
import { ExplorerConfigProvider } from ".contexts/ExplorerConfigContext";
import SignInPage from "./pages/SignInPage";
import NavigationPage from "./pages/NavigationPage";
import ExplorerPage from "./explorer/pages/ExplorerPage";

function App() {
  useEffect(() => {
    const handleBeforeUnload = (event) => {
      // You can perform synchronous cleanup here.
      // For example, logging or saving state to localStorage.
      console.log("Before unload: resetting state or sending a beacon");

      // If you want to prompt the user (modern browsers limit this)
      // event.preventDefault();
      // event.returnValue = "";
    };

    window.addEventListener("beforeunload", handleBeforeUnload);

    // Cleanup the listener when App unmounts.
    return () => {
      window.removeEventListener("beforeunload", handleBeforeUnload);
    };
  }, []);

  console.log("App.js");
  return (
    <UserSessionProvider>
      <Router>
        <Routes>
          <Route path="/" element={<SignInPage />} />
          <Route path="/navigation" element={<NavigationPage />} />  
            <Route path="/explorer" 
              element={
                <ExplorerConfigProvider>
                  <ExplorerPage />
                </ExplorerConfigProvider>
              }
            />
        </Routes>
      </Router>
    </UserSessionProvider>
  );
}

export default App;
