SELECT
'PS'::text as gear
,extract(year from r.date)::integer as year
,o.label1::text as ocean
,co.iso3code::text AS flag
,p.label1::text as program
,t.startdate::date as trip_start_date
,v.label1::text as vessel
,ob.lastname::text as observer
,r.date::date as observation_date
,a.time::time as observation_time
,a.latitude::numeric as latitude
,a.longitude::numeric as longitude
,STRING_AGG(os.code, ';')::text as observed_system_code
,STRING_AGG(os.label1, ';')::text as observed_system_label
,(CASE WHEN st.code='0' THEN 'UNK' ELSE (CASE WHEN st.code='1' THEN 'FOB' ELSE (CASE WHEN st.code='2' THEN 'FSC' END) END) END)::text as school_type
,c2.faocode::text as fao_code
,s.topiaid::text as set_id

FROM ps_common.trip t
INNER JOIN ps_common.program p ON (t.observationsprogram = p.topiaid)
INNER JOIN common.ocean o ON (t.ocean = o.topiaid)
INNER JOIN common.person ob ON (t.observer = ob.topiaid)
INNER JOIN common.vessel v ON (t.vessel = v.topiaid)
INNER JOIN common.country co ON (v.flagcountry = co.topiaid)
INNER JOIN ps_observation.route r ON (r.trip = t.topiaid)
INNER JOIN ps_observation.activity a ON (a.route = r.topiaid)
INNER JOIN ps_common.vesselactivity va ON (a.vesselactivity = va.topiaid)
LEFT OUTER JOIN ps_observation.activity_observedsystem aos ON (aos.activity = a.topiaid)
LEFT OUTER JOIN ps_common.observedsystem os ON (aos.observedsystem = os.topiaid)
LEFT OUTER JOIN ps_observation.set s ON (s.activity = a.topiaid)
LEFT OUTER JOIN ps_common.schooltype st ON (s.schooltype = st.topiaid)
LEFT OUTER JOIN
	(
	SELECT
	STRING_AGG(sp.faocode, ';') as faocode,
	STRING_AGG(sg.label1, ';') as speciesgroup,
	c.set
	FROM ps_observation.catch c
	INNER JOIN common.species sp ON (c.species = sp.topiaid)
	INNER JOIN common.speciesgroup sg ON (sp.speciesgroup = sg.topiaid)
	GROUP BY
	c.set
	)
	as c2 ON (c2.set = s.topiaid)

WHERE
extract(year from r.date) between (?start_year) and (?end_year)
AND o.label1 in (?ocean)
AND p.topiaid in (?program)
AND va.code LIKE '6'
AND (os.code IN ('9','10','11','12','21','22','111','112') OR c2.faocode LIKE '%RHN%' OR c2.speciesgroup LIKE '%Cetaceans%')
AND co.iso3code in (?country_code)

GROUP BY
p.label1,
r.date,
o.label1,
v.label1,
co.iso3code,
t.startdate,
ob.lastname,
a.time,
a.latitude,
a.longitude,
s.schooltype,
s.topiaid,
st.code,
c2.faocode

ORDER BY
o.label1,
t.startdate,
r.date

;
