SELECT * FROM communes;
SELECT * FROM departements;
SELECT * FROM regions;

SELECT *
FROM departements
    JOIN regions ON regions.id = departements.reg
WHERE regions.nom = 'Corse';

SELECT pop
FROM communes
WHERE nom = 'Lyon'
LIMIT 1;

SELECT COUNT(*), SUM(pop), AVG(pop) FROM communes;

SELECT nom, pop FROM communes
ORDER BY pop DESC
LIMIT 10;

-- 4
SELECT COUNT(*) FROM communes
WHERE pop > 50_000;

-- 5
SELECT COUNT(DISTINCT nom) FROM communes;

-- 6
SELECT nom, pop FROM communes
WHERE pop < length(nom);

-- 7
SELECT c.nom, c.pop
FROM communes AS c
    JOIN departements AS d ON c.dep = d.id
WHERE d.nom = 'Meuse'
ORDER BY c.pop ASC
LIMIT 10;

-- 8
SELECT d.nom, SUM(c.pop)
FROM departements AS d
    JOIN communes AS c ON c.dep = d.id
GROUP BY d.id
ORDER BY COUNT(c.id) DESC
LIMIT 10;

-- 9
SELECT r.nom, SUM(pop)
FROM regions AS r
    JOIN departements AS d ON d.reg = r.id
    JOIN communes AS c ON c.dep = d.id
GROUP BY r.id
ORDER BY SUM(pop) DESC
LIMIT 10;

-- Exercice 4
-- 1
SELECT r.nom, COUNT(c.id) AS nb_communes
FROM communes AS c
    JOIN departements AS d ON c.dep = d.id
    JOIN regions AS r ON d.reg = r.id
WHERE c.pop < 100
GROUP BY r.id
ORDER BY nb_communes DESC
LIMIT 1;

-- 2
SELECT nom, COUNT(id) AS nb
FROM communes
GROUP BY nom
ORDER BY nb DESC
LIMIT 1;

-- 3
SELECT d.nom, c.nom
FROM communes AS c
    JOIN departements as d ON c.dep = d.id
GROUP BY d.id, c.nom
HAVING COUNT(c.id) >= 2;

-- 4
SELECT c.id, c.nom, c.pop, SUM(c.pop)
FROM communes AS c
    JOIN communes AS autre_c ON
        c.nom = autre_c.nom
        AND c.id <> autre_c.id
GROUP BY c.id
ORDER BY c.pop DESC
LIMIT 1;

-- 5
SELECT d.nom
FROM departements AS d
WHERE NOT d.id IN (
    SELECT dep
    FROM communes
    WHERE pop < 20
)
ORDER BY d.nom;

SELECT d.nom
FROM departements AS d
EXCEPT
SELECT DISTINCT d.nom
FROM communes AS c
    JOIN departements AS d ON d.id = c.dep
WHERE c.pop < 20
ORDER BY d.nom;

-- Exercice 5
-- 1: Donne le nom, la population et le département
--    des villes les plus peuplées de chaque département
-- 2: La complexité serait en O(n^2). Ça prendrait beaucoup de temps
-- 3:
SELECT c.nom, c.pop, c.dep
FROM communes AS c
JOIN
    (SELECT MAX(pop) AS maxp, dep
     FROM communes
     GROUP BY dep)
    AS d
ON c.dep = d.dep
WHERE c.pop = d.maxp;
