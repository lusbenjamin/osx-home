ALTER TABLE lead ADD COLUMN salesforce_id character varying(255);
ALTER TABLE lead ADD CONSTRAINT "lead_salesforce_id_key" UNIQUE (salesforce_id);
ALTER TABLE activity ADD COLUMN salesforce_id character varying(255);
ALTER TABLE activity ADD CONSTRAINT "activity_salesforce_id_key" UNIQUE (salesforce_id);
ALTER TABLE atype ADD COLUMN salesforce_id character varying(255);
ALTER TABLE atype ADD CONSTRAINT "atype_salesforce_id_key" UNIQUE (salesforce_id);

/*
-- Count number of conversions we ingested
select attributes->>'New Value' as conv_name,
    count(distinct lead_id) as conv_cnt
from activity
where description = 'Change Data Value'
    and value = 'Funnel Stage'
    and occurred_at >= '2016-02-01'
    and occurred_at <= '2016-02-29'
group by attributes->>'New Value'
order by attributes->>'New Value';

-- Count number of conversions we extracted
select ce.conversion_id,
    c.name,
    count(distinct ce.lead_id) as conv_cnt,
    sum(ca.credit) as credit
from conversion_event ce
join conversion c on c.id = ce.conversion_id
join campaign_attribution ca on ca.conversion_event_id = ce.id
where 1 = 1
    and occurred_at >= '2016-02-01'
    and occurred_at <= '2016-02-29'
group by ce.conversion_id, c.name
order by c.name;


--INSERT INTO activity (occurred_at, lead_id, campaign_id, description, atype_id, attributes)
SELECT conversion_event.occurred_at, conversion_event.lead_id
FROM conversion_event
LEFT OUTER JOIN (
     SELECT activity.lead_id AS lead_id, activity.occurred_at AS occurred_at, activity.campaign_id AS campaign_id
     FROM activity
     JOIN (SELECT *
        FROM campaign
        WHERE campaign.description IN ('Organic', 'New Lead', 'Open Email', 'Visit Webpage', 'Fill Out Form', 'Fill Out Form Hubspot')
     ) AS v_cpgs
        ON v_cpgs.id = activity.campaign_id
) AS v_act
   ON v_act.lead_id = conversion_event.lead_id
   AND v_act.occurred_at <= conversion_event.occurred_at
--WHERE v_act.campaign_id IS NULL
AND conversion_event.lead_id = 36
limit 1;
*/

/*
alter table "integration"
add column user_id int
    constraint integration_user_id_fkey
    references "user" (id) on delete cascade;


alter table "integration"
add column refresh_token char varying(255);


alter table "integration"
add constraint integration_unique_key
    unique (org_id, flavor);
*/

/*
update activity
set campaign_id = null
where campaign_id in
(
	select id from campaign
	where description = 'Change Data Value'
);

delete from campaign
where description = 'Change Data Value';
*/
/*

SELECT ce.id, ce.conversion_id
FROM conversion_event ce
LEFT JOIN (
select a.id, a.lead_id, a.occurred_at, a.campaign_id
from activity a
JOIN (
        SELECT campaign.id AS id, campaign.parent_id AS parent_id, campaign.level AS level, campaign.start_date AS start_date, campaign.end_date AS end_date, campaign.name AS name, campaign.display_name AS display_name, campaign.description AS description, campaign.attributes AS attributes, campaign.cnt AS cnt
        FROM campaign
        WHERE campaign.description IN (NULL, NULL, 'New Lead', 'Open Email', 'Visit Webpage', 'Fill Out Form', 'Fill Out Form Hubspot')) AS vc
        on vc.id = a.campaign_id
) va
    ON va.lead_id = ce.lead_id
    AND va.occurred_at <= ce.occurred_at
WHERE va.id IS NULL
    and ce.id in (2196, 2300)
LIMIT 15
;
*/

/*
ALTER TABLE campaign ADD COLUMN start_date TIMESTAMP WITHOUT TIME ZONE;
ALTER TABLE campaign ADD COLUMN end_date TIMESTAMP WITHOUT TIME ZONE;
ALTER TABLE campaign ADD COLUMN display_name CHARACTER VARYING(255);
ALTER TABLE campaign ADD CONSTRAINT campaign_display_name_key UNIQUE (display_name);
UPDATE campaign SET display_name = name;
*/


/*
select distinct value
from activity
where description = 'Change Data Value'
;
select distinct value,
       attributes->>'Old Value' as old_value,
       attributes->>'New Value' as new_value
from activity
where description = 'Change Data Value'
     and value = 'Lead Status'
order by old_value, new_value
;
select distinct value,
       attributes->>'Old Value' as old_value,
       attributes->>'New Value' as new_value
from activity
where description = 'Change Data Value'
     and value = 'Lead Status'
order by old_value, new_value
;
select distinct value,
       attributes->>'Old Value' as old_value,
       attributes->>'New Value' as new_value
from activity
where description = 'Change Data Value'
     and value = 'Lead Score'
order by old_value, new_value
;
select value,
       count(*) as cnt
from activity
where description = 'Change Data Value'
     and value in ('Lead Score', 'Relative Score', 'Lead Status')
group by value
;
*/
/*
;
select value,
       count(*) as cnt
from activity
where description = 'Change Data Value'
     and value in ('Funnel Stage', 'Lead Status')
group by value
*/
/*
;
select attributes->>'New Value' as new_value,
       count(*) as cnt
from activity
where description = 'Change Data Value'
     and value = 'Lead Status'
group by new_value
order by cnt desc
*/
/*
DROP TABLE lead_conversion;
DROP TABLE campaign_attribution;
DROP TABLE first_touch;
DROP TABLE conversion_path;
*/


/*
INSERT INTO conversion
VALUES (0, 0, 'beta', 'beta');
*/

--CREATE INDEX ix_activity_occurred_at ON activity (occurred_at);

/*
;
;
SELECT * FROM first_touch
ORDER BY lead_id, occurred_at
LIMIT 25;

*/

/*
-- Create new activity extract
-- Records first time a lead encounters a campaign
DROP TABLE first_touch
;
CREATE TABLE first_touch
(
  id SERIAL CONSTRAINT first_touch_id_key UNIQUE
  , occurred_at TIMESTAMP NOT NULL
  , lead_id INT NOT NULL CONSTRAINT first_touch_lead_id_fkey
      REFERENCES lead (id)
  , campaign_id INT NOT NULL CONSTRAINT first_touch_campaign_id_fkey
      REFERENCES campaign (id)
  , is_first_ever BOOLEAN
  , is_last_ever BOOLEAN
  , CONSTRAINT uq_first_touch_min_touch UNIQUE (lead_id, is_first_ever)
  , CONSTRAINT uq_first_touch_max_touch UNIQUE (lead_id, is_last_ever)
  , CONSTRAINT uq_first_touch_campaign_id UNIQUE (lead_id, campaign_id)
)
;
CREATE INDEX ix_first_touch_occurred_at
       ON first_touch (occurred_at)
;
AND a.lead_id IN
(
  99545,
  39784,
 100095,
   8054,
 105884
)
*/
/*
SELECT MIN(ft.occurred_at) as min_dt
       , MAX(ft.occurred_at) as max_dt
       , ft.lead_id
FROM first_touch ft
GROUP BY ft.lead_id
LIMIT 25;
*/
/*
SELECT ft.lead_id
       , limits.min_dt
       , limits.max_dt
       , CASE WHEN (ft.occurred_at = limits.min_dt) THEN True
       ELSE NULL
       END AS is_first_ever
       , CASE WHEN (ft.occurred_at = limits.max_dt) THEN True
       ELSE NULL
       END AS is_last_ever
FROM first_touch ft
JOIN
(
SELECT MIN(ft.occurred_at) as min_dt
       , MAX(ft.occurred_at) as max_dt
       , ft.lead_id
FROM first_touch ft
GROUP BY ft.lead_id
) limits
  ON limits.lead_id = ft.lead_id
*/
/*
;
SELECT ft.campaign_id
       , SUM(CASE WHEN ft.is_first_ever THEN 1 END) as min_cnt
       , SUM(CASE WHEN ft.is_last_ever THEN 1 END) as max_cnt
       , SUM(CASE WHEN ft.is_first_ever IS NULL AND ft.is_last_ever IS NULL THEN 1 END) as mid_cnt
       , COUNT(lead_id)
FROM first_touch ft
GROUP BY ft.campaign_id
*/
;
-- TOP 5 min_cnt
/*
22,
82,
87,
86,
15
*/
-- TOP 5 max_cnt
/*
82,
91,
93,
83,
87
*/
-- TOP 5 mid_cnt
/*
82,
90,
87,
91,
93
*/
/*
;
DROP TABLE conversion_path
;
CREATE TABLE conversion_path
(
  id SERIAL CONSTRAINT conversion_path_id_key UNIQUE
  , occurred_at TIMESTAMP NOT NULL
  , lead_id INT NOT NULL CONSTRAINT conversion_path_lead_id_fkey
      REFERENCES lead (id)
  , source_cpg_id INT NULL CONSTRAINT conversion_path_source_cpg_id_fkey
      REFERENCES campaign (id)
  , target_cpg_id INT NOT NULL CONSTRAINT conversion_path_target_cpg_id_fkey
      REFERENCES campaign (id)
  , edge_type INT
  -- Initially enforce linear paths (no trees, no DAGS, no bitters)
  -- Doubles as indexing for aggregation queries (key order matters)
  -- TODO revisit order of keys after writing queries
  , CONSTRAINT uq_conversion_path_lead_source UNIQUE (source_cpg_id, lead_id)
  , CONSTRAINT uq_conversion_path_lead_target UNIQUE (target_cpg_id, lead_id)
  , CONSTRAINT uq_conversion_path_lead_ts UNIQUE (occurred_at, lead_id)
)
;
*/
/*
;
SELECT cp.source_cpg_id
       , c.name
       , cp.target_cpg_id
       , c2.name
       , COUNT(cp.lead_id) as flow
FROM conversion_path cp
JOIN campaign c
     ON c.id = cp.source_cpg_id
JOIN campaign c2
     ON c2.id = cp.target_cpg_id
WHERE cp.edge_type = 1
      AND cp.source_cpg_id != 82
      AND cp.target_cpg_id != 82
GROUP BY cp.source_cpg_id
      , c.name
      , cp.target_cpg_id
      , c2.name
HAVING COUNT(cp.lead_id) > 2
ORDER BY flow DESC
;

SELECT cp.source_cpg_id
       , c.name
       , cp.target_cpg_id
       , c2.name
       , COUNT(cp.lead_id) as flow
FROM conversion_path cp
JOIN campaign c
     ON c.id = cp.source_cpg_id
JOIN campaign c2
     ON c2.id = cp.target_cpg_id
WHERE cp.edge_type = 3
      AND cp.source_cpg_id != 82
      AND cp.target_cpg_id != 82
GROUP BY cp.source_cpg_id
      , c.name
      , cp.target_cpg_id
      , c2.name
HAVING COUNT(cp.lead_id) > 2
ORDER BY flow DESC
LIMIT 25
;
*/

/*



WHERE cp.is_final

LIMIT 25


;
SELECT *
FROM first_touch ft
JOIN conversion_path cp
GROUP BY
*/


/*
SELECT campaign_id,
       lead_id,
       occurred_at,
       dense_rank(lead_id, occurred_at) WITHIN GROUP
           (ORDER BY lead_id DESC, occurred_at DESC) AS rnk
FROM activity
WHERE campaign_id IS NOT NULL
GROUP BY campaign_id, lead_id, occurred_at
HAVING rnk < 2
LIMIT 20;
*/


/* cool leads that converted
SELECT *
FROM lead_conversion lc
WHERE lc.lead_id IN
(
  1549,
  99545,
  39784,
 100095,
   8054,
 105884,
  68712
)

  id  | lead_id |     occurred_at
------+---------+---------------------
 2112 |   39784 | 2015-09-15 15:44:21
 2388 |  105884 | 2015-08-20 17:27:39
 2407 |  100095 | 2015-07-28 17:14:14
 2423 |   99545 | 2015-07-27 14:40:20
 4530 |    8054 | 2015-10-19 12:31:39

*/

/* quick look at potential paths
SELECT
  a.lead_id
  , a.occurred_at
  , a.campaign_id
  , c.name
  , c.description
FROM activity a
JOIN campaign c
  ON c.id = a.campaign_id
WHERE a.lead_id IN
(
  1549,
  99545,
  39784,
 100095,
   8054,
 105884,
  68712
)
ORDER BY a.lead_id, a.occurred_at
*/

/* potentially cool leads

SELECT l.id
  , COUNT(DISTINCT a.campaign_id) as cpg_cnt
FROM lead l
JOIN activity a
  ON a.lead_id = l.id
GROUP BY l.id
ORDER BY cpg_cnt DESC


   1549 |       5
  99545 |       5
  39784 |       5
 100095 |       5
   8054 |       5
 105884 |       5
  68712 |       5


*/




/*
-- for killing zombies
SELECT pg_terminate_backend(pid)
FROM pg_stat_activity
WHERE pid <> pg_backend_pid()
      AND state = 'idle';
*/


/*
TODO run on production after next deployment

DROP TABLE campaign_rollup;
DROP TABLE campaign_daily;
DROP TABLE score_segment;
DROP TABLE active_segment;
*/


/*
ALTER TABLE lead
ADD COLUMN hubspot_id UUID NULL CONSTRAINT lead_hubspot_id_key UNIQUE;

ALTER TABLE lead
ALTER COLUMN created_at SET DEFAULT now();

ALTER TABLE lead
ALTER COLUMN updated_at SET DEFAULT now();

ALTER TABLE activity
ADD COLUMN hubspot_id UUID NULL CONSTRAINT activity_hubspot_id_key UNIQUE;

ALTER TABLE activity
ADD COLUMN atype_id int NULL;

CREATE INDEX ix_activity_atype_id ON activity (atype_id);
*/
/*
does not have sequence
CREATE TABLE atype
(
    id int PRIMARY KEY,
    mkto_id int NULL CONSTRAINT atype_mkto_id_key UNIQUE,
    hubspot_id int NULL CONSTRAINT atype_hubspot_id_key UNIQUE,
    name text NOT NULL DEFAULT '',
    description text NOT NULL DEFAULT ''
)
*/


--TRUNCATE TABLE temp_cpg_trend_domain_;
/*
SELECT c.id,
       c.name,
       l.id,
       l.email,
       l.description,
       l.mkto_id,
       l.valid,
       l.complete,
       a.rnk,
       a.occurred_at
FROM campaign AS c
JOIN
(
SELECT campaign_id,
       lead_id,
       occurred_at,
       dense_rank(1, 2) OVER (PARTITION BY campaign_id
              ORDER BY lead_id DESC, occurred_at DESC) AS rnk
FROM activity
WHERE campaign_id IS NOT NULL
) AS a
    ON a.campaign_id = c.id
        AND a.rnk < 5
JOIN lead AS l
    ON l.id = a.lead_id
LIMIT 20;

SELECT campaign_id,
       lead_id,
       occurred_at,
       dense_rank(lead_id, occurred_at) WITHIN GROUP
           (ORDER BY lead_id DESC, occurred_at DESC) AS rnk
FROM activity
WHERE campaign_id IS NOT NULL
GROUP BY campaign_id, lead_id, occurred_at
HAVING rnk < 2
LIMIT 20;

SELECT campaign_id,
    lead_id,
    occurred_at,
    rank() OVER (PARTITION BY campaign_id
                 ORDER BY lead_id DESC, occurred_at DESC) AS rnk
FROM activity
WHERE campaign_id IS NOT NULL
LIMIT 20;


EXPLAIN ANALYZE
SELECT *
FROM lead
LIMIT 10;
*/
