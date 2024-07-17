SELECT
o.label1 AS ocean
,p.label1 AS PROGRAM
,extract(year from r.date)::integer as year
,v.label1 AS vessel
,co.label1 AS flag_country
,ob.lastname AS observer
,t.startdate AS trip_start_date
,t.enddate AS trip_end_date
,r.date::date AS observation_date
,a.time AS observation_time
,va.label1 AS vessel_activity
,va.code AS vessel_activity_code
,a.latitude AS latitude
,a.longitude AS longitude
,t.topiaid AS trip_id
,a.topiaid AS activity_id
,a.lastupdatedate AS lastupdate_date
,fo.computedwhenarrivingsimplifiedobjecttype AS object_type_arriving
,fo.computedwhenleavingsimplifiedobjecttype AS object_type_leaving
,op.label1 AS object_operation
,STRING_AGG(om.label1, ', ') AS object_feature


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
LEFT OUTER JOIN ps_common.reasonfornullset rns ON (s.reasonfornullset = rns.topiaid)
LEFT OUTER JOIN common.fpazone fpa ON (a.currentfpazone = fpa.topiaid)
LEFT OUTER JOIN ps_observation.floatingobject fo ON fo.activity = a.topiaid
INNER JOIN ps_common.objectoperation op ON fo.objectoperation = op.topiaid
INNER JOIN ps_observation.floatingobjectpart fop ON fop.floatingobject = fo.topiaid
LEFT OUTER JOIN ps_common.objectmaterial om ON om.topiaid = fop.objectmaterial 

WHERE
extract(year from t.startdate) between (?start_year) and (?end_year)
AND p.topiaid in (?program)
AND o.label1 in (?ocean)
AND co.iso3code in (?country_code)

GROUP BY 
o.label1
,o.code
,p.label1
,v.label1
,co.label1
,ob.lastname
,t.startdate
,t.enddate
,r.date
,a.time
,va.label1
,va.code
,a.latitude
,a.longitude
,t.topiaid
,a.topiaid
,a.lastupdatedate
,fo.computedwhenarrivingsimplifiedobjecttype
,fo.computedwhenleavingsimplifiedobjecttype
,op.label1
