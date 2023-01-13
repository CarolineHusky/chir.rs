CREATE TABLE auth_users (
    id TEXT PRIMARY KEY NOT NULL,
    password_file BYTEA NOT NULL,
    activated BOOLEAN NOT NULL
);
CREATE TABLE auth_authenticators (
    id BYTEA PRIMARY KEY NOT NULL,
    user_id TEXT NOT NULL,
    webauthn_registration TEXT NOT NULL,
    FOREIGN KEY (user_id) references auth_users(id) ON DELETE CASCADE
);
CREATE TABLE auth_user_sessions (
    jti TEXT PRIMARY KEY NOT NULL,
    user_id TEXT NOT NULL,
    exp_at TIMESTAMP WITH TIME ZONE NOT NULL,
    reauth_after TIMESTAMP WITH TIME ZONE,
    FOREIGN KEY (user_id) references auth_users(id) ON DELETE CASCADE
);
CREATE TABLE auth_session_scopes (
    jti TEXT NOT NULL,
    scope TEXT NOT NULL,
    FOREIGN KEY (jti) references auth_user_sessions(jti) ON DELETE CASCADE,
    PRIMARY KEY (jti, scope)
);
CREATE TABLE auth_kv (
    kv_key BYTEA PRIMARY KEY NOT NULL,
    kv_value BYTEA NOT NULL
);
