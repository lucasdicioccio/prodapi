-- Add backbone user-identification tables.

-- users and other identification
CREATE TABLE identities
(
	id bigserial,
        PRIMARY KEY (id)
);

-- users with passwords
CREATE TABLE passwords
(
	email VARCHAR(255) NOT NULL,
	enabled BOOLEAN NOT NULL,
	hashed VARCHAR(255) NOT NULL,
	salt VARCHAR(255) NOT NULL,
	user_id bigint,
	UNIQUE (email),
        FOREIGN KEY (user_id) REFERENCES identities(id)
);

-- lost-password requests
CREATE TABLE password_lost_request
(
	id bigserial,
	timestamp TIMESTAMPTZ NOT NULL,
	user_id bigint,
	token VARCHAR(255) NOT NULL,
	used_at TIMESTAMPTZ,
        FOREIGN KEY (user_id) REFERENCES identities(id),
        PRIMARY KEY (id)
);
