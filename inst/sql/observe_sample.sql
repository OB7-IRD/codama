SELECT
p.label1::text as program
,extract(year from r.date)::integer as year
,o.label1::text as ocean
,v.label1::text as vessel
,t.homeid::text as home_id
,t.startdate::date as trip_start_date
,t.enddate::date as trip_end_date
,ob.lastname::text as observer
,r.date::date as observation_date
,a.time::time as observation_time
,a.latitude::numeric as latitude
,a.longitude::numeric as longitude
,(CASE WHEN st.code='0' THEN 'UNK' ELSE (CASE WHEN st.code='1' THEN 'FOB' ELSE (CASE WHEN st.code='2' THEN 'FSC' END) END) END)::text as school_type
,sg.label1::text as species_group
,sp.faocode::text as fao_code
,sp.label1::text as common_name
,sp.scientificlabel::text as scientific_name
,sm.count::integer count
,sm.weight::numeric as weight
,sm.isweightcomputed::boolean as weight_was_computed
,sm.length::numeric as length
,sm.islengthcomputed::boolean as length_was_computed
,smt.code::text as size_type
,sx.label1::text as sex
,sf.label1::text as fate
,sf.code::integer as fate_code
,sf.topiaid::text as fate_id
,sa.comment::text as sample_comment
,t.topiaid::text as trip_id
,s.topiaid::text as set_id
,sa.topiaid::text as sample_id
,sm.topiaid::text as samplemeasure_id

FROM ps_common.trip t
INNER JOIN ps_common.program p ON (t.observationsprogram = p.topiaid)
INNER JOIN common.ocean o ON (t.ocean = o.topiaid)
INNER JOIN common.person ob ON (t.observer = ob.topiaid)
INNER JOIN common.vessel v ON (t.vessel = v.topiaid)
INNER JOIN common.country co ON (v.flagcountry = co.topiaid)
INNER JOIN ps_observation.route r ON (r.trip = t.topiaid)
INNER JOIN ps_observation.activity a ON (a.route = r.topiaid)
INNER JOIN ps_common.vesselactivity va ON (a.vesselactivity = va.topiaid)
INNER JOIN ps_observation.set s ON (s.activity = a.topiaid)
INNER JOIN ps_observation.sample sa ON (sa.set = s.topiaid)
INNER JOIN ps_observation.samplemeasure sm ON (sm.sample = sa.topiaid)
INNER JOIN common.sizemeasuretype smt ON (sm.sizemeasuretype = smt.topiaid)
INNER JOIN common.species sp ON (sm.species = sp.topiaid) 
INNER JOIN common.speciesgroup sg ON (sp.speciesgroup = sg.topiaid)
LEFT OUTER JOIN common.sex sx ON (sm.sex = sx.topiaid)
LEFT OUTER JOIN ps_common.speciesfate sf ON (sm.speciesfate = sf.topiaid)
LEFT OUTER JOIN ps_common.schooltype st ON (s.schooltype = st.topiaid)

WHERE
extract(year from r.date) between (?start_year) and (?end_year)
AND p.topiaid in (?program)
AND o.label1 in (?ocean)
AND co.iso3code in (?country_code)

;
