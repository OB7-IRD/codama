SELECT
o.label1 AS ocean
,p.label1 AS program
,extract(year from t.startdate)::integer AS year
,v.label1 AS vessel
,co.label1 AS flag_country
,ob.lastname AS observer
,t.startdate::date AS trip_start_date
,t.enddate::date AS trip_end_date
,t.observationscomment AS observations_comment
--t.generalcomment AS general_comment,
,t.topiaid AS trip_id

FROM ps_common.trip t
INNER JOIN ps_common.program p ON (t.observationsprogram = p.topiaid)
INNER JOIN common.ocean o ON (t.ocean = o.topiaid)
INNER JOIN common.person ob ON (t.observer = ob.topiaid)
INNER JOIN common.vessel v ON (t.vessel = v.topiaid)
INNER JOIN common.country co ON (v.flagcountry = co.topiaid)

WHERE
extract(year from t.startdate) between (?start_year) and (?end_year)
AND p.topiaid in (?program)
AND o.label1 in (?ocean)
AND co.iso3code in (?country_code)
