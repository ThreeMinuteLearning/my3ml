-- -*- mode: sql; sql-product: postgres; -*-
CREATE TYPE user_type AS ENUM ('Student', 'Teacher', 'SchoolAdmin', 'Editor', 'Admin');

CREATE TYPE event_type AS ENUM ('LoginSuccess', 'LoginFailure');

CREATE TYPE dict_entry AS (word text, index smallint);

CREATE EXTENSION "uuid-ossp";
CREATE EXTENSION "pgcrypto";

CREATE TABLE login
    ( id uuid DEFAULT uuid_generate_v4() PRIMARY KEY
    , username text NOT NULL UNIQUE CHECK (length(username) > 0)
    , password text NOT NULL CHECK (length(password) > 0)
    , user_type user_type DEFAULT 'Student' NOT NULL
    , locked boolean NOT NULL DEFAULT false
    , active boolean NOT NULL DEFAULT false
    , settings jsonb
    , otp_key text
    , last_login timestamptz
    , created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP
    );

CREATE TABLE user_keys
    ( user_id uuid PRIMARY KEY REFERENCES login(id) ON DELETE CASCADE
    , salt bytea NOT NULL
    , pub_key jsonb NOT NULL
    , priv_key text NOT NULL
    , school_key text
    );

CREATE TABLE story
    ( id serial PRIMARY KEY
    , title text NOT NULL
    , img_url text NOT NULL
    , level smallint NOT NULL CHECK (level >= 0 AND level < 10)
    , qualification text CHECK (length(qualification) > 0)
    , curriculum text CHECK (length(curriculum) > 0)
    , tags text[] NOT NULL
    , content text NOT NULL CHECK (length(content) > 0)
    , words dict_entry[] NOT NULL
    , clarify_word text NOT NULL
    , enabled boolean DEFAULT false
    , archived boolean DEFAULT false
    , created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP
    , CONSTRAINT clarify_word_check CHECK (length(clarify_word) > 1 AND (position(lower(clarify_word) in lower(content)) > 0 OR position(lower(clarify_word) in lower(title)) > 0 OR position(' ' in clarify_word) > 0))
    );

CREATE TABLE school
    ( id uuid DEFAULT uuid_generate_v4() PRIMARY KEY
    , name text NOT NULL CHECK (length(name) > 0)
    , description text
    , school_key text
    , created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP
    );

CREATE TABLE class
    ( id uuid PRIMARY KEY
    , name text NOT NULL CHECK (length(name) > 0)
    , description text
    , school_id uuid NOT NULL REFERENCES school
    , created_by uuid NOT NULL REFERENCES login ON DELETE CASCADE
    , UNIQUE (id, school_id)
    , UNIQUE (name, school_id)
    );

CREATE TABLE teacher
    ( id uuid PRIMARY KEY REFERENCES login(id) ON DELETE CASCADE
    , name text NOT NULL CHECK (length(name) > 0)
    , bio text
    , school_id uuid REFERENCES school
    , created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP
    );

CREATE TABLE student
    ( id uuid PRIMARY KEY REFERENCES login(id) ON DELETE CASCADE
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

CREATE TABLE anthology
    ( id uuid DEFAULT uuid_generate_v4() PRIMARY KEY
    , name text NOT NULL CHECK (length(name) > 0)
    , description text NOT NULL
    , school_id uuid REFERENCES school
    , stories integer[] NOT NULL
    , hidden boolean NOT NULL default false
    , created_by uuid NOT NULL REFERENCES login ON DELETE CASCADE
    , created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP
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
    , school_id uuid NOT NULL REFERENCES school ON DELETE CASCADE
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
    , school_id uuid NOT NULL REFERENCES school ON DELETE CASCADE
    , created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP
    );

CREATE TABLE famous_name
    ( name text PRIMARY KEY
    );

CREATE TABLE config
    ( starter_stories uuid REFERENCES anthology
    );

CREATE MATERIALIZED VIEW leaderboard
AS
    SELECT row_number() OVER (ORDER BY a.score DESC) AS position
         , l.username as name
         , s.id as student_id
         , s.school_id
         , a.score
      FROM student s
      JOIN ( SELECT student_id, 200 + 100 * count(*) as score
               FROM story_answer
               GROUP BY student_id
           ) a
      ON a.student_id = s.id
      JOIN login l
      ON l.id = s.id
      WHERE NOT s.hidden
      ORDER BY a.score DESC
WITH NO DATA;

CREATE UNIQUE INDEX ON leaderboard (student_id, school_id);

CREATE OR REPLACE FUNCTION refresh_leaderboard() RETURNS void
SECURITY DEFINER
AS $$
  REFRESH MATERIALIZED VIEW leaderboard with data;
$$ LANGUAGE sql;


CREATE OR REPLACE FUNCTION array_sum(integer[]) RETURNS bigint AS
$$
  SELECT sum(unnest) FROM (select unnest($1)) AS blah
$$
LANGUAGE sql;


CREATE OR REPLACE FUNCTION to_epoch(timestamptz) RETURNS integer AS
$$
  SELECT round(extract(epoch FROM $1)) :: integer
$$ LANGUAGE SQL;


CREATE OR REPLACE FUNCTION get_story_activity_history(period interval, granularity text)
  RETURNS json AS
$$
  WITH story_activity AS (
    SELECT s.name AS school_name
         , s.id AS school_id
         , d.dt
         , count(sa.story_id) AS stories
      FROM school s
      CROSS JOIN generate_series(date_trunc(granularity, now() - period)
                               , date_trunc(granularity, now())
                               , (1 || granularity)::interval) d (dt)
      LEFT JOIN story_answer sa
        ON sa.school_id = s.id
        AND sa.created_at >= d.dt
        AND sa.created_at < d.dt + (1 || granularity)::interval
    GROUP BY s.id, s.name, d.dt
    ORDER BY s.id, s.name, d.dt
  ), schools_with_data AS (
    SELECT school_id, sum(stories) FROM story_activity GROUP BY school_id HAVING sum(stories) > 0
  ), story_activity_by_school AS (
    SELECT school_name, stories, dt as date FROM story_activity sa INNER JOIN schools_with_data swd ON sa.school_id = swd.school_id
  )
  SELECT json_agg(t) FROM story_activity_by_school t
$$ LANGUAGE sql;


CREATE OR REPLACE FUNCTION story_popularity()
  RETURNS json AS
$$
  WITH story_activity AS (
    SELECT sc.name AS school_name
        , s.title AS story_title
        , count(student.id) AS students
    FROM story_answer sa
    INNER JOIN student
    ON student.id = sa.student_id
    INNER JOIN school sc
    ON sc.id = sa.school_id
    INNER JOIN story s
    ON s.id = sa.story_id
    GROUP BY sc.name, s.title
    ORDER BY sc.name, students DESC
  )
  SELECT json_agg(t) FROM story_activity t
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION school_data()
  RETURNS json AS
$$
  WITH teachers AS (
    SELECT s.id AS school_id, json_build_object('name', t.name, 'email', l.username, 'last_login', to_epoch(l.last_login), 'created_at', to_epoch(l.created_at)) AS teacher_data
    FROM login l
    INNER JOIN teacher t
    ON t.id = l.id
    INNER JOIN school s
    ON s.id = t.school_id
  ), teachers_per_school AS (
    SELECT school_id, json_agg(teacher_data) AS teachers
    FROM teachers
    GROUP BY school_id
  ), student_counts AS (
    SELECT s.id AS school_id, s.name AS school_name, s.created_at AS created_at, count(student.id) AS number_of_students
    FROM school s
    LEFT JOIN student
    ON s.id = student.school_id
    GROUP BY s.id, s.name, s.created_at
  )
  SELECT json_agg(row_to_json(tmp))
  FROM (
    SELECT sc.school_name AS name, to_epoch(sc.created_at) AS created_at, t.teachers, sc.number_of_students
    FROM teachers_per_school t
    INNER JOIN student_counts sc
    ON t.school_id = sc.school_id
  ) tmp
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION dashboard()
  RETURNS json AS
$$
  WITH m AS (
    SELECT get_story_activity_history(interval '24 months', 'month') AS story_activity_monthly
  ), d AS (
    SELECT get_story_activity_history(interval '8 weeks', 'day') AS story_activity_daily
  ), tm AS (
    SELECT to_epoch(now()) AS sample_time
  ), school_data AS (
    SELECT school_data() as schools
  ), sp AS (
    SELECT story_popularity()
  )
  SELECT row_to_json(t, true)
  FROM (
    SELECT * FROM tm, m, d, sp, school_data
  ) t
$$ LANGUAGE sql;
