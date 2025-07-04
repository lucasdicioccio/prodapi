
CREATE TABLE IF NOT EXISTS task_queue (
    id SERIAL PRIMARY KEY,
    not_before TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    not_after TIMESTAMP,
    priority numeric NOT NULL DEFAULT 100,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    status TEXT NOT NULL,
    payload jsonb
);

