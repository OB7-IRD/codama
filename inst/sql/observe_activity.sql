SELECT
o.label1 AS ocean
,p.label1 AS program
,extract(year from r.date)::integer as year
,v.label1 AS vessel
,co.label1 AS flag_country
,ob.lastname AS observer
,t.startdate::date AS trip_start_date
,t.enddate::date AS trip_end_date
,r.date::date AS observation_date
,a.time AS observation_time
,va.label1 AS vessel_activity
,va.code AS vessel_activity_code
,a.latitude AS latitude
,a.longitude AS longitude
,fpa.label1 AS fpa_zone
,fpa.code AS fpa_zone_code
,(CASE WHEN st.code='0' THEN 'UNK' ELSE (CASE WHEN st.code='1' THEN 'FOB' ELSE (CASE WHEN st.code='2' THEN 'FSC' END) END) END) AS school_type
,rns.label1 AS reason_null_set
,rns.code AS reason_null_set_code
,s.comment AS set_comment
,t.homeid AS trip_homeid
,t.topiaid AS trip_id
,s.topiaid AS set_id

FROM ps_common.trip t
INNER JOIN ps_common.program p ON (t.observationsprogram = p.topiaid)
INNER JOIN common.ocean o ON (t.ocean = o.topiaid)
INNER JOIN common.person ob ON (t.observer = ob.topiaid)
INNER JOIN common.vessel v ON (t.vessel = v.topiaid)
INNER JOIN common.country co ON (v.flagcountry = co.topiaid)
INNER JOIN ps_observation.route r ON (r.trip = t.topiaid)
INNER JOIN ps_observation.activity a ON (a.route = r.topiaid)
INNER JOIN ps_common.vesselactivity va ON (a.vesselactivity = va.topiaid)
LEFT OUTER JOIN ps_observation.set s ON (s.activity = a.topiaid)
LEFT OUTER JOIN ps_common.schooltype st ON (s.schooltype = st.topiaid)
LEFT OUTER JOIN ps_common.reasonfornullset rns ON (s.reasonfornullset = rns.topiaid)
LEFT OUTER JOIN common.fpazone fpa ON (a.currentfpazone = fpa.topiaid)

WHERE
extract(year from r.date) between (?start_year) and (?end_year)
AND p.topiaid in (?program)
AND o.label1 in (?ocean)
AND co.iso3code in (?country_code)
