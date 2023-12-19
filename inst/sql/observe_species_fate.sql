SELECT
sf.label1::text as fate
,sf.code::integer as fate_code
,sf.topiaid::text as fate_id

FROM ps_common.speciesfate sf
;
