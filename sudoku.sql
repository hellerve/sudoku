-- sqlite3 < sudoku.sql

WITH RECURSIVE
  puzzle(s) AS (
    VALUES ('306508400'
         || '520000000'
         || '087000031'
         || '003010080'
         || '900863005'
         || '050090600'
         || '130000250'
         || '000000074'
         || '005206300')
  ),

  seq(n) AS (VALUES (0) UNION ALL SELECT n + 1 FROM seq WHERE n < 80),

  pos(p, r, c, b) AS MATERIALIZED (
    SELECT n, n / 9, n % 9, n / 27 * 3 + n % 9 / 3 FROM seq
  ),

  digits(z) AS MATERIALIZED (
    SELECT CAST(n + 1 AS TEXT) FROM seq WHERE n < 9
  ),

  search(s, p, holes) AS (
    SELECT s,
           (SELECT m.p
            FROM pos m LEFT JOIN digits d
              ON NOT EXISTS (
                SELECT 1 FROM pos q
                WHERE (q.r = m.r OR q.c = m.c OR q.b = m.b)
                  AND substr(s, q.p + 1, 1) = d.z)
            WHERE substr(s, m.p + 1, 1) = '0'
            GROUP BY m.p
            ORDER BY count(d.z), m.p
            LIMIT 1),
           length(s) - length(replace(s, '0', ''))
    FROM puzzle
    UNION ALL
    SELECT substr(s, 1, e.p) || z.z || substr(s, e.p + 2),
           (SELECT m.p
            FROM pos m LEFT JOIN digits d
              ON NOT EXISTS (
                SELECT 1 FROM pos q
                WHERE (q.r = m.r OR q.c = m.c OR q.b = m.b)
                  AND substr(substr(s, 1, e.p) || z.z || substr(s, e.p + 2),
                             q.p + 1, 1) = d.z)
            WHERE substr(substr(s, 1, e.p) || z.z || substr(s, e.p + 2),
                         m.p + 1, 1) = '0'
            GROUP BY m.p
            ORDER BY count(d.z), m.p
            LIMIT 1),
           holes - 1
    FROM search, pos e, digits z
    WHERE e.p = search.p
      AND NOT EXISTS (
        SELECT 1 FROM pos q
        WHERE (q.r = e.r OR q.c = e.c OR q.b = e.b)
          AND substr(s, q.p + 1, 1) = z.z)
    ORDER BY 3
  ),

  solution(s) AS MATERIALIZED (
    SELECT s FROM search WHERE holes = 0 LIMIT 1
  ),

  fmt(i, o) AS (SELECT n, (n - 1 - n / 4) * 9 FROM seq WHERE n < 13)

SELECT CASE WHEN i % 4 = 0 THEN '-------------------------'
            ELSE printf('| %s %s %s | %s %s %s | %s %s %s |',
                        substr(s, o + 1, 1), substr(s, o + 2, 1),
                        substr(s, o + 3, 1), substr(s, o + 4, 1),
                        substr(s, o + 5, 1), substr(s, o + 6, 1),
                        substr(s, o + 7, 1), substr(s, o + 8, 1),
                        substr(s, o + 9, 1))
       END
FROM fmt, solution
UNION ALL
SELECT 'Unsolvable' WHERE NOT EXISTS (SELECT 1 FROM solution);
