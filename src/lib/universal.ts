// src/lib/universal.ts
import { db } from "@/db/client";

export function getFullUniversal(userId = 1) {
  return db.universal.findMany({
    where: { userId },
    orderBy: { date: "desc" },    // optional: most recent first
  });
}