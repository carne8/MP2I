-- Exercice 2
-- 1
SeLecT nom, prenom FROM peintres
ORDER BY nom, prenom;

-- 2
SeLecT DISTINCT nom FROM musees
ORDER BY nom;

-- 3
SeLecT nom, prenom FROM peintres
WHERE prenom = 'Michelangelo'
ORDER BY nom, prenom;

-- 4
SeLecT nom_commun, nom, prenom FROM peintres
WHERE nom_commun IS NOT NULL
ORDER BY nom_commun;

-- 5
SELECT nom, prenom FROM peintres
WHERE
    annee_mort - annee_naissance >= 70 AND id_pays = 1
    OR
    annee_naissance >= 1700 AND annee_mort <= 1799;

-- 6
SeLecT nom_commun FROM peintres
WHERE nom_commun IS NOT NULL
UNION
SeLecT nom FROM peintres;

-- 7
SeLecT p1.annee_mort
FROM peintres p1
    JOIN peintres p2 ON p1.annee_mort = p2.annee_naissance;

SeLecT annee_naissance FROM peintres
INTERSECT
SeLecT annee_mort FROM peintres;

-- 8
SELECT e.nom, a.nom
FROM oeuvres e, peintres a
WHERE e.id_peintre = a.id;

SELECT e.nom, a.nom
FROM oeuvres e JOIN peintres a ON e.id_peintre = a.id;

-- 9
SELECT e.nom
FROM oeuvres e
    JOIN musees m ON e.id_musee = m.id
WHERE m.nom = 'Musée du Louvre';

-- 10
SELECT m.id, m.nom
FROM musees m
    JOIN oeuvres o ON o.id_musee = m.id
    JOIN peintres p ON o.id_peintre = p.id
WHERE p.nom_commun = 'Le Titien'
GROUP BY m.id;

SELECT DISTINCT m.id, m.nom
FROM musees m
    JOIN oeuvres o ON o.id_musee = m.id
    JOIN peintres p ON o.id_peintre = p.id
WHERE p.nom_commun = 'Le Titien';

-- 11
SELECT p.nom, m.nom
FROM musees m
    JOIN oeuvres o ON o.id_musee = m.id
    JOIN peintres p ON p.id = o.id_peintre
    JOIN villes v ON m.id_ville = v.id AND v.id_pays = 1
GROUP BY p.id, m.id
ORDER BY lower(p.nom), lower(m.nom);