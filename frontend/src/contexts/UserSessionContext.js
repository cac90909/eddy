import React, { createContext, useContext, useState } from "react";

// Create the UserContext
const UserContext = createContext();

// Create a custom hook to access the context easily
export const useUser = () => useContext(UserContext);

// Create a provider component
export const UserSessionProvider = ({ children }) => {
console.log("UserProvider")
  const [userId, setUserId] = useState("");

  return (
    <UserContext.Provider value={{ userId, setUserId }}>
      {children}
    </UserContext.Provider>
  );
};
