CREATE TYPE user_type AS ENUM ('Student', 'Teacher', 'Admin');

CREATE TYPE dict_entry AS (word text, index smallint);

CREATE EXTENSION "uuid-ossp";

CREATE TABLE login
    ( id uuid DEFAULT uuid_generate_v4() PRIMARY KEY
    , username text NOT NULL UNIQUE CHECK (length(username) > 0)
    , password text NOT NULL CHECK (length(password) > 0)
    , user_type user_type DEFAULT 'Student' NOT NULL
    , otp_key text
    );

CREATE TABLE story
    ( id text PRIMARY KEY
    , title text NOT NULL
    , img_url text NOT NULL
    , level smallint NOT NULL CHECK (level >= 0 AND level < 10)
    , curriculum text NOT NULL CHECK (length(curriculum) > 0)
    , tags text[] NOT NULL
    , content text NOT NULL CHECK (length(content) > 0)
    , words dict_entry[] NOT NULL
    , created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP
    );

CREATE TABLE school
    ( id uuid PRIMARY KEY
    , name text NOT NULL CHECK (length(name) > 0)
    , description text
    );

CREATE TABLE class
    ( id uuid PRIMARY KEY
    , name text NOT NULL CHECK (length(name) > 0)
    , description text
    , school_id uuid NOT NULL REFERENCES school
    , created_by uuid NOT NULL REFERENCES login
    );

CREATE TABLE teacher
    ( id uuid PRIMARY KEY
    , sub uuid NOT NULL REFERENCES login
    , name text NOT NULL CHECK (length(name) > 0)
    , bio text
    , school_id uuid REFERENCES school
    , created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP
    );

CREATE TABLE student
    ( id uuid PRIMARY KEY
    , name text NOT NULL CHECK (length(name) > 0)
    , description text
    , level smallint NOT NULL CHECK (level >= 0 AND level < 10)
    , sub uuid NOT NULL REFERENCES login
    , school_id uuid NOT NULL REFERENCES school
    , created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP
    );

CREATE TABLE student_class
    ( student_id uuid NOT NULL REFERENCES student
    , class_id uuid NOT NULL REFERENCES class
    );

CREATE TABLE trail
    ( id uuid PRIMARY KEY
    , name text NOT NULL CHECK (length(name) > 0)
    , school_id uuid NOT NULL REFERENCES school
    , stories uuid[] NOT NULL
    );

CREATE TABLE dict
    ( word text NOT NULL
    , index smallint NOT NULL check (index >= 0 AND index < 20)
    , definition text NOT NULL
    , uses_words dict_entry[] NOT NULL
    );
