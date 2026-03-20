SELECT id,
    name,
    a,
    b,
    c,
    dcasdfasdfasdf,
    asdfasdfasdf,
    asdfasdfasdfa,
    asdfasdfasdfasdfasdfasdf
FROM users
INNER JOIN whatever
    ON a = b
WHERE id = '1224'
    AND (a = b OR b = CASE b WHEN 1 THEN 2 END)
