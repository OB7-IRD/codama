--- Recovers information about the species caught, the ocean declared in the trip
SELECT 
    c.topiaid::text AS topiaid_catch, 
    o.label1::text AS ocean, 
    s.label1::text AS species, 
    s.topiaid::text AS topiaid_species,
    so.ocean::text AS ocean_species_ocean, 
    so.species::text AS species_species_ocean
FROM 
    ps_logbook.catch c 
    INNER JOIN common.species s ON c.species = s.topiaid 
    INNER JOIN ps_logbook.activity a ON c.activity = a.topiaid 
    INNER JOIN ps_logbook.route r ON a.route = r.topiaid 
    INNER JOIN ps_common.trip t ON r.trip = t.topiaid 
    INNER JOIN common.ocean o ON t.ocean = o.topiaid 
    LEFT JOIN common.species_ocean so ON so.ocean= o.topiaid AND so.species = s.topiaid 
WHERE 
    t.topiaid IN (?trip)
