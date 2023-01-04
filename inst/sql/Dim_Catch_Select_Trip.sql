-- Number of catches for the trip
SELECT count(c.topiaid)::numeric
FROM ps_logbook.catch c 
INNER JOIN ps_logbook.activity a ON c.activity = a.topiaid 
INNER JOIN ps_logbook.route r ON a.route = r.topiaid 
INNER JOIN ps_common.trip t ON r.trip = t.topiaid 
WHERE 
    t.topiaid IN (?trip)
