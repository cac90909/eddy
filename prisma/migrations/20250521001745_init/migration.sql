-- CreateTable
CREATE TABLE "auth_user" (
    "id" INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
    "email" TEXT NOT NULL
);

-- CreateTable
CREATE TABLE "universal" (
    "id" INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
    "date" DATETIME NOT NULL,
    "functionalities" JSONB,
    "subject_matters" JSONB,
    "general_categories" JSONB,
    "title" TEXT,
    "text" TEXT,
    "tags" JSONB,
    "parents_ids" JSONB,
    "children_ids" JSONB,
    "siblings_ids" JSONB,
    "fields" JSONB,
    "entry_id" TEXT NOT NULL,
    "userId" INTEGER NOT NULL,
    CONSTRAINT "universal_userId_fkey" FOREIGN KEY ("userId") REFERENCES "auth_user" ("id") ON DELETE RESTRICT ON UPDATE CASCADE
);

-- CreateTable
CREATE TABLE "snapshots" (
    "snapshot_id" INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
    "userId" INTEGER NOT NULL,
    "title" TEXT NOT NULL,
    "description" TEXT NOT NULL,
    "operation_chain" JSONB NOT NULL DEFAULT [],
    "created_at" DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" DATETIME NOT NULL,
    CONSTRAINT "snapshots_userId_fkey" FOREIGN KEY ("userId") REFERENCES "auth_user" ("id") ON DELETE RESTRICT ON UPDATE CASCADE
);

-- CreateIndex
CREATE UNIQUE INDEX "auth_user_email_key" ON "auth_user"("email");

-- CreateIndex
CREATE UNIQUE INDEX "universal_entry_id_key" ON "universal"("entry_id");
