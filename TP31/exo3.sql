-- 1
SELECT prenom, nom, annee_mort - annee_naissance
FROM peintres
WHERE annee_mort - annee_naissance < 40;

-- 2: Affiche les oeuvres et leur musée où elles sont exposées
--    si le nom du musée fait moins de 20 caractères et au'il
--    commence par 'Musée'

-- 3
SELECT prenom || ' ' || nom nom_complet
FROM peintres
WHERE prenom LIKE '%a%' 
    AND nom NOT LIKE 'Ve%'
    AND nom LIKE '%i_';

-- 4
SELECT COUNT(o.id)
FROM musees m
    JOIN oeuvres o ON o.id_musee = m.id
WHERE m.nom = 'Musée du Louvre';

-- 5
SELECT SUM(LENGTH(m.nom))
FROM musees m
    JOIN villes v ON m.id_ville = v.id
WHERE v.id_pays <> 1;

-- 6
SELECT COUNT(id)
FROM peintres
WHERE annee_naissance > (
    SELECT AVG(annee_naissance) FROM peintres
);

-- 7
SELECT nom, annee_naissance
FROM peintres
WHERE annee_naissance >= (
    SELECT MAX(annee_naissance) FROM peintres
);

-- 8
SELECT * FROM oeuvres
WHERE id_musee IS NULL;

-- 9
SELECT o.nom, m.nom FROM oeuvres o
LEFT JOIN musees m ON o.id_musee = m.id;

-- 10
SELECT nom FROM musees
ORDER BY -LENGTH(nom);

-- 11
SELECT o.nom, p.nom
FROM oeuvres o
    JOIN peintres p ON o.id_peintre = p.id
ORDER BY p.annee_naissance DESC, o.nom;

-- 12
SELECT * FROM pays
ORDER BY LOWER(nom)
OFFSET 2
LIMIT 3;

-- 13
SELECT p.id, p.nom, COUNT(o.id)
FROM peintres p
    JOIN oeuvres o ON o.id_peintre = p.id
GROUP BY p.id;

-- 14
SELECT p.id, p.nom, COUNT(DISTINCT m.id)
FROM peintres p
    JOIN oeuvres o ON o.id_peintre = p.id
    JOIN musees m ON o.id_musee = m.id
GROUP BY p.id;

-- 15
SELECT p.nom, COUNT(peintre.id), AVG(annee_mort - annee_naissance)
FROM pays p
    JOIN peintres peintre ON peintre.id_pays = p.id
GROUP BY p.id;

-- 16
SELECT m.nom, COUNT(p.id)
FROM musees m
    JOIN oeuvres o ON o.id_musee = m.id
    JOIN peintres p ON p.id = o.id_peintre
WHERE p.annee_naissance > 1480
GROUP BY m.id
HAVING m.nom LIKE 'G%' AND COUNT(p.id) >= 2;