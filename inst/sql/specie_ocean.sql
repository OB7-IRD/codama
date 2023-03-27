-- Retrieves information on species and their associated ocean
SELECT 
    so.ocean::text AS ocean_id, 
    so.species::text AS specie_id,
    o.label1::text AS ocean_name 
FROM 
    common.species_ocean so
    INNER JOIN common.ocean o ON so.ocean = o.topiaid
