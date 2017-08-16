CREATE TYPE user_type AS ENUM ('Student', 'Teacher', 'SchoolAdmin', 'Editor', 'Admin');

CREATE TYPE dict_entry AS (word text, index smallint);

CREATE EXTENSION "uuid-ossp";

CREATE TABLE login
    ( id uuid DEFAULT uuid_generate_v4() PRIMARY KEY
    , username text NOT NULL UNIQUE CHECK (length(username) > 0)
    , password text NOT NULL CHECK (length(password) > 0)
    , user_type user_type DEFAULT 'Student' NOT NULL
    , locked boolean NOT NULL DEFAULT false
    , active boolean NOT NULL DEFAULT false
    , settings jsonb
    , otp_key text
    , created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP
    );

CREATE TABLE story
    ( id serial PRIMARY KEY
    , title text NOT NULL
    , img_url text NOT NULL
    , level smallint NOT NULL CHECK (level >= 0 AND level < 10)
    , qualification text NOT NULL CHECK (length(qualification) > 0)
    , curriculum text CHECK (length(curriculum) > 0)
    , tags text[] NOT NULL
    , content text NOT NULL CHECK (length(content) > 0)
    , words dict_entry[] NOT NULL
    , clarify_word text NOT NULL CHECK (length(clarify_word) > 0)
    , created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP
    );

CREATE TABLE school
    ( id uuid DEFAULT uuid_generate_v4() PRIMARY KEY
    , name text NOT NULL CHECK (length(name) > 0)
    , description text
    );

CREATE TABLE class
    ( id uuid PRIMARY KEY
    , name text NOT NULL CHECK (length(name) > 0)
    , description text
    , school_id uuid NOT NULL REFERENCES school
    , created_by uuid NOT NULL REFERENCES login
    , UNIQUE (id, school_id)
    );

CREATE TABLE teacher
    ( id uuid PRIMARY KEY REFERENCES login(id)
    , name text NOT NULL CHECK (length(name) > 0)
    , bio text
    , school_id uuid REFERENCES school
    , created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP
    );

CREATE TABLE student
    ( id uuid PRIMARY KEY REFERENCES login(id)
    , name text NOT NULL CHECK (length(name) > 0)
    , description text
    , level smallint NOT NULL CHECK (level >= 0 AND level <= 10)
    , school_id uuid NOT NULL REFERENCES school
    , hidden boolean NOT NULL default false
    , deleted timestamptz
    , created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP
    , UNIQUE (id, school_id)
    );

CREATE TABLE student_class
    ( student_id uuid NOT NULL
    , class_id uuid NOT NULL
    , school_id uuid NOT NULL
    , PRIMARY KEY (student_id, class_id)
    , FOREIGN KEY (student_id, school_id) REFERENCES student (id, school_id) ON DELETE CASCADE
    , FOREIGN KEY (class_id, school_id) REFERENCES class (id, school_id) ON DELETE CASCADE
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
    , sensitive boolean NOT NULL default false
    , PRIMARY KEY (word, index)
    );

CREATE TABLE story_answer
    ( story_id integer NOT NULL REFERENCES story
    , student_id uuid NOT NULL REFERENCES student
    , school_id uuid NOT NULL REFERENCES school
    , connect text NOT NULL
    , question text NOT NULL
    , summarise text NOT NULL
    , clarify text NOT NULL
    , hidden boolean NOT NULL default false
    , created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP
    , PRIMARY KEY (student_id, story_id)
    );

CREATE TABLE registration_code
    ( code text PRIMARY KEY
    , school_id uuid NOT NULL REFERENCES school
    , created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP
    );

CREATE MATERIALIZED VIEW leaderboard
AS
    SELECT row_number() OVER (ORDER BY a.score DESC) AS position
         , s.name
         , s.id as student_id
         , s.school_id
         , a.score
      FROM student s
      JOIN ( SELECT student_id, 200 + 100 * count(*) as score
               FROM story_answer
               GROUP BY student_id
           ) a
      ON a.student_id = s.id
      WHERE not s.hidden
      ORDER BY a.score DESC
WITH NO DATA;

CREATE UNIQUE INDEX ON leaderboard (student_id, school_id);

CREATE OR REPLACE FUNCTION refresh_leaderboard() RETURNS void
SECURITY DEFINER
AS $$
BEGIN
    REFRESH MATERIALIZED VIEW leaderboard with data;

    RETURN;
END;
$$ LANGUAGE plpgsql;
