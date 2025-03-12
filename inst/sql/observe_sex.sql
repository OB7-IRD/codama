SELECT
sex.label1::text as sex
,sex.code::integer as sex_code
,sex.topiaid::text as sex_id

FROM common.sex sex
;
