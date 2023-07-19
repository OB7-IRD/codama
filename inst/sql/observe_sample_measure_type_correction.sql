SELECT
sp.faocode::text as fao_code
,sp.scientificlabel::text as scientific_name
,sp.label1::text as common_name
,sp.label2::text as nom_commun
,sg.label1::text as species_group
,sm.length::numeric as length
,smt.code::text as size_type
,sm.islengthcomputed::text as length_was_computed
,sm.weight::numeric as weight
,sm.isweightcomputed::text as weight_was_computed
,sm.count::integer count
,sx.label1::text as sex
,fa.code::integer as fate_code
,fa.label1::text as fate
,o.label1::text as ocean
,'PS'::text as gear
,p.label1::text as program
,a.latitude::numeric as latitude
,a.longitude::numeric as longitude
,extract(year from r.date)::integer as year
,t.startdate::date as trip_start_date
,t.enddate::date as trip_end_date
,v.label1::text as vessel
,ob.lastname::text as observer
,r.date::date as observation_date
,a.time::time as observation_time
,(CASE WHEN st.code='0' THEN 'UNK' ELSE (CASE WHEN st.code='1' THEN 'FOB' ELSE (CASE WHEN st.code='2' THEN 'FSC' END) END) END)::text as school_type
,sa.comment::text as sample_comment
,t.topiaid::text as trip_id
,t.homeid::text as homeid
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
LEFT OUTER JOIN ps_common.speciesfate fa ON (sm.speciesfate = fa.topiaid)
LEFT OUTER JOIN ps_common.schooltype st ON (s.schooltype = st.topiaid)

WHERE
extract(year from r.date) between (?start_year) and (?end_year)
AND p.topiaid in (?program)
AND o.label1 in (?ocean)
AND co.iso3code in (?country_code)
AND va.code = '6'
AND sp.faocode in (?species)
AND sg.label1 in (?species_group)
AND smt.code in (?sizemeasuretype_to_replace)



;
