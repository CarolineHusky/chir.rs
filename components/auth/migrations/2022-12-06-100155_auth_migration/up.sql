CREATE TABLE users (
    id TEXT PRIMARY KEY NOT NULL,
    password_file BYTEA NOT NULL
);
CREATE TABLE authenticators (
    id INTEGER GENERATED ALWAYS AS IDENTITY,
    user_id TEXT NOT NULL,
    webauthn_registration TEXT NOT NULL,
    PRIMARY KEY (id),
    FOREIGN KEY (user_id) references users(id) ON DELETE CASCADE
);
CREATE TABLE user_sessions (
    jti TEXT PRIMARY KEY NOT NULL,
    user_id TEXT NOT NULL,
    exp_at TIMESTAMP WITH TIME ZONE NOT NULL,
    reauth_after TIMESTAMP WITH TIME ZONE,
    FOREIGN KEY (user_id) references users(id) ON DELETE CASCADE
);
CREATE TABLE session_scopes (
    jti TEXT NOT NULL,
    scope TEXT NOT NULL,
    FOREIGN KEY (jti) references user_sessions(jti) ON DELETE CASCADE,
    PRIMARY KEY (jti, scope)
);
