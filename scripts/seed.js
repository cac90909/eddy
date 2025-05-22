// scripts/seed.js
import fs from "fs";
import path from "path";
import { parse } from "csv-parse/sync";
import { PrismaClient } from "@prisma/client";

console.log("🔍 Starting CSV-backed seed…");
const db = new PrismaClient();

// 1) Load & parse the CSV into an array of row‐objects
const csv = fs.readFileSync(path.join(process.cwd(), "scripts/data.csv"), "utf8");
const records = parse(csv, {
  columns: true,           // first row → object keys
  skip_empty_lines: true,
  trim: true
});

// Helpers
function parseArray(cell) {
  if (!cell) return undefined;
  return cell.split(";").map((s) => s.trim()).filter(Boolean);
}
function parseFields(cell) {
  if (!cell) return undefined;
  return Object.fromEntries(
    cell.split("--").map((pair) => {
      const idx = pair.indexOf(":");
      const key   = pair.slice(0, idx).trim();
      const raw   = pair.slice(idx + 1).trim();
      const val   = raw.toUpperCase() === "NULL" ? null : raw;
      return [key, val];
    })
  );
}

async function main() {
  // 2) Reset old data
  await db.universal.deleteMany();

  // 3) Ensure a seed user exists
  const user = await db.user.upsert({
    where:  { email: "seed@test.com" },
    update: {},
    create: { email: "seed@test.com" },
  });

  // 4) Iterate CSV rows by column name
  for (const row of records) {
    await db.universal.create({
      data: {
        userId: user.id,
        date: new Date(row["Date"]),
        functionalities:    parseArray(row["Functionalities"]),
        subject_matters:    parseArray(row["Subject Matters"]),
        general_categories: parseArray(row["General Categories"]),
        title:   row["Title"] || undefined,
        text:    row["Text"]  || undefined,
        tags:    parseArray(row["Tags"]),
        parents_ids:  parseArray(row["Parents IDs"]),
        children_ids: parseArray(row["Children IDs"]),
        siblings_ids: parseArray(row["Siblings IDs"]),
        fields:       parseFields(row["Fields"]),
        entry_id:     row["Entry ID"]
      }
    });
  }

  console.log("✅ Seed complete!");
  await db.$disconnect();
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
