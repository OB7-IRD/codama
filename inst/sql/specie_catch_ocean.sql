--- Recovers information about the species caught, the ocean declared in the trip
SELECT 
    c.topiaid::text AS catch_id, 
    o.topiaid::text AS ocean_id, 
    s.topiaid::text AS specie_id,
    s.label1::text AS specie_name, 
    o.label1::text AS ocean_name

FROM 
    ps_logbook.catch c 
    INNER JOIN common.species s ON c.species = s.topiaid 
    INNER JOIN ps_logbook.activity a ON c.activity = a.topiaid 
    INNER JOIN ps_logbook.route r ON a.route = r.topiaid 
    INNER JOIN ps_common.trip t ON r.trip = t.topiaid 
    INNER JOIN common.ocean o ON t.ocean = o.topiaid 
WHERE 
    t.topiaid IN (?select_item) OR
    EXTRACT(year FROM t.startdate) IN (?select_item) OR EXTRACT(year FROM t.enddate) IN (?select_item)
