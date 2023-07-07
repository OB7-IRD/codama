SELECT
sp.faocode::text as fao_code
,smt.code::text as default_measure_type
,o.label1::text as ocean
,sp.minlength::numeric as min_length
,sp.maxlength::numeric as max_length

FROM common.species sp
INNER JOIN common.sizemeasuretype smt on (sp.sizemeasuretype = smt.topiaid)
INNER JOIN common.species_ocean so on (sp.topiaid = so.species)
INNER JOIN common.ocean o on (so.ocean = o.topiaid)

;
