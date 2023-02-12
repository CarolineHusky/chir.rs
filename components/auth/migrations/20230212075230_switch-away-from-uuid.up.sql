ALTER TABLE auth_session_scopes DROP CONSTRAINT auth_session_scopes_pkey;
ALTER TABLE auth_session_scopes DROP CONSTRAINT auth_session_scopes_jti_fkey;
ALTER TABLE auth_user_sessions DROP CONSTRAINT auth_user_sessions_pkey;
ALTER TABLE auth_user_sessions
ALTER COLUMN jti
SET DATA TYPE TEXT;
ALTER TABLE auth_session_scopes
ALTER COLUMN jti
SET DATA TYPE TEXT;
ALTER TABLE auth_user_sessions
ADD CONSTRAINT auth_user_sessions_pkey PRIMARY KEY (jti);
ALTER TABLE auth_session_scopes
ADD CONSTRAINT auth_session_scopes_jti_fkey FOREIGN KEY (jti) REFERENCES auth_user_sessions(jti) ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE auth_session_scopes
ADD CONSTRAINT auth_session_scopes_pkey PRIMARY KEY (jti, scope);
