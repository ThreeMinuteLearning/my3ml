DO $$
  DECLARE test_school_id uuid;
BEGIN
  SELECT school.id INTO test_school_id FROM school WHERE name = 'Monkey Test School';
  DELETE FROM class WHERE school_id = test_school_id;
  DELETE FROM student WHERE school_id = test_school_id;
  DELETE FROM login WHERE id IN (SELECT id FROM teacher WHERE teacher.school_id = test_school_id);
  DELETE FROM registration_code WHERE school_id = test_school_id;
  DELETE FROM school WHERE id = test_school_id;
END $$;
