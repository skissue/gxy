-- Add migration script here
CREATE TABLE todos (
       id INTEGER PRIMARY KEY,
       text TEXT NOT NULL,
       done BOOLEAN NOT NULL
)
