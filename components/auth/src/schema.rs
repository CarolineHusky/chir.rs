// @generated automatically by Diesel CLI.

diesel::table! {
    authenticators (id) {
        id -> Int4,
        user_id -> Text,
        webauthn_registration -> Text,
    }
}

diesel::table! {
    session_scopes (jti, scope) {
        jti -> Text,
        scope -> Text,
    }
}

diesel::table! {
    user_sessions (jti) {
        jti -> Text,
        user_id -> Text,
        exp_at -> Timestamptz,
        reauth_after -> Nullable<Timestamptz>,
    }
}

diesel::table! {
    users (id) {
        id -> Text,
        password_file -> Bytea,
    }
}

diesel::joinable!(authenticators -> users (user_id));
diesel::joinable!(session_scopes -> user_sessions (jti));
diesel::joinable!(user_sessions -> users (user_id));

diesel::allow_tables_to_appear_in_same_query!(
    authenticators,
    session_scopes,
    user_sessions,
    users,
);
