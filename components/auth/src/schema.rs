// @generated automatically by Diesel CLI.

diesel::table! {
    auth_authenticators (id) {
        id -> Bytea,
        user_id -> Text,
        webauthn_registration -> Text,
    }
}

diesel::table! {
    auth_session_scopes (jti, scope) {
        jti -> Text,
        scope -> Text,
    }
}

diesel::table! {
    auth_user_sessions (jti) {
        jti -> Text,
        user_id -> Text,
        exp_at -> Timestamptz,
        reauth_after -> Nullable<Timestamptz>,
    }
}

diesel::table! {
    auth_users (id) {
        id -> Text,
        password_file -> Bytea,
        activated -> Bool,
    }
}

diesel::joinable!(auth_authenticators -> auth_users (user_id));
diesel::joinable!(auth_session_scopes -> auth_user_sessions (jti));
diesel::joinable!(auth_user_sessions -> auth_users (user_id));

diesel::allow_tables_to_appear_in_same_query!(
    auth_authenticators,
    auth_session_scopes,
    auth_user_sessions,
    auth_users,
);
