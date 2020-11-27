-- Add backbone identity-identification tables.

-- identitys and other identification
CREATE TABLE identities
(
	id bigserial,
        PRIMARY KEY (id)
);

-- identitys with passwords
CREATE TABLE passwords
(
	email VARCHAR(255) NOT NULL,
	enabled BOOLEAN NOT NULL,
	hashed VARCHAR(255) NOT NULL,
	salt VARCHAR(255) NOT NULL,
	identity_id bigint,
	UNIQUE (email),
        FOREIGN KEY (identity_id) REFERENCES identities(id)
);

-- lost-password requests
CREATE TABLE password_lost_request
(
	id bigserial,
	timestamp TIMESTAMPTZ NOT NULL,
	identity_id bigint,
	token VARCHAR(255) NOT NULL,
	used_at TIMESTAMPTZ,
        FOREIGN KEY (identity_id) REFERENCES identities(id),
        PRIMARY KEY (id)
);
