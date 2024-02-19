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
,c.totalcount::integer as count
,c.meanweight::numeric as mean_individual_weight
,(CASE WHEN c.meanweightcomputedsource IS NULL THEN 'observed' ELSE 'computed' END)::text as is_mean_individual_weight_kg_computed
,c.meanlength::numeric as mean_individual_length_cm
,(CASE WHEN c.meanlengthcomputedsource IS NULL THEN 'observed' ELSE 'computed' END)::text as is_mean_individual_length_cm_computed
,c.catchweight::numeric as weight_tons
,i.label1::text as information_source
,(CASE WHEN c.totalcountcomputedsource IS NULL THEN 'observed' ELSE 'computed' END)::text as is_number_computed
,sf.label1::text as fate
,sf.code::integer as fate_code
,sf.topiaid::text as fate_id
,rfd.label1::text as reason_discarded
,c.comment::text as catch_comment
,s.topiaid::text as set_id
,c.topiaid::text as catch_id

FROM ps_common.trip t
INNER JOIN ps_common.program p on (t.observationsprogram = p.topiaid)
INNER JOIN common.ocean o on (t.ocean = o.topiaid)
INNER JOIN common.person ob on (t.observer = ob.topiaid)
INNER JOIN common.vessel v on (t.vessel = v.topiaid)
INNER JOIN common.country co on (v.flagcountry = co.topiaid)
INNER JOIN ps_observation.route r on (r.trip = t.topiaid)
INNER JOIN ps_observation.activity a on (a.route = r.topiaid)
INNER JOIN ps_observation.set s on (s.activity = a.topiaid)
INNER JOIN ps_observation.catch c on (c.set = s.topiaid)
INNER JOIN common.species sp on (c.species = sp.topiaid)
INNER JOIN common.speciesgroup sg on (sp.speciesgroup = sg.topiaid)
LEFT OUTER JOIN ps_common.speciesfate sf on (c.speciesfate = sf.topiaid)
LEFT OUTER JOIN ps_observation.reasonfordiscard rfd on (c.reasonfordiscard = rfd.topiaid)
LEFT OUTER JOIN ps_common.schooltype st on (s.schooltype = st.topiaid)
LEFT OUTER JOIN ps_observation.informationsource i on (c.informationsource = i.topiaid)

WHERE
extract(year from r.date) between (?start_year) and (?end_year)
AND p.topiaid in (?program)
AND o.label1 in (?ocean)
AND co.iso3code in (?country_code)

;

