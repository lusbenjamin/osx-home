ALTER TABLE lead DROP CONSTRAINT lead_hubspot_vid_key;
ALTER TABLE lead DROP CONSTRAINT lead_hubspot_id_key;
ALTER TABLE lead DROP CONSTRAINT lead_mkto_id_key;
ALTER TABLE lead DROP CONSTRAINT lead_salesforce_id_key;

CREATE INDEX ix_lead_hubspot_id ON lead (hubspot_id);
CREATE INDEX ix_lead_mkto_id ON lead (mkto_id);
CREATE INDEX ix_lead_salesforce_id ON lead (salesforce_id);


ALTER TABLE lead ADD COLUMN primary_email text;
ALTER TABLE lead ADD COLUMN email_pattern text;

CREATE UNIQUE INDEX lead_primary_email_key ON lead (primary_email text_pattern_ops);
CREATE UNIQUE INDEX lead_email_pattern_key ON lead (email_pattern text_pattern_ops);

UPDATE lead
SET primary_email = lead_rnk.email,
    email_pattern = lead_rnk.email
FROM (
    SELECT id, email,
        row_number() OVER (
            PARTITION BY email
            ORDER BY char_length(email_pattern) DESC, updated_at DESC) as rnk
    FROM lead
) lead_rnk
WHERE lead.id = lead_rnk.id
    AND lead_rnk.rnk = 1;

TRUNCATE TABLE conversion_event CASCADE;
TRUNCATE TABLE campaign_activity CASCADE;

UPDATE activity
SET lead_id = link.new_id
FROM (
SELECT lead_a.id AS old_id, lead_b.id AS new_id
FROM lead AS lead_a
JOIN lead AS lead_b
    ON lead_b.email_pattern = lead_a.email
    AND lead_b.id != lead_a.id
) link
WHERE activity.lead_id = link.old_id;

SELECT count(*)
FROM activity
WHERE lead_id IN (
SELECT id
FROM lead
WHERE primary_email IS NULL
);

UPDATE lead
SET mkto_id = CASE WHEN lead.mkto_id IS NOT NULL THEN lead.mkto_id
              ELSE link.mkto_id END,
    hubspot_id = CASE WHEN lead.hubspot_id IS NOT NULL THEN lead.hubspot_id
              ELSE link.hubspot_id END,
    salesforce_id = CASE WHEN lead.salesforce_id IS NOT NULL THEN lead.salesforce_id
              ELSE link.salesforce_id END,
    description = CASE WHEN char_length(lead.description) > char_length(link.description)
                  THEN lead.description ELSE link.description END
FROM (
SELECT lead_a.id AS old_id,
    lead_b.id AS new_id,
    lead_a.mkto_id,
    lead_a.hubspot_id,
    lead_a.salesforce_id,
    lead_a.description
FROM lead AS lead_a
JOIN lead AS lead_b
    ON lead_b.email_pattern = lead_a.email
    AND lead_b.id != lead_a.id
) link
WHERE lead.id = link.new_id;

SELECT *
INTO lead_purge
FROM lead
WHERE primary_email IS NULL;

INSERT INTO lead_purge
SELECT *
FROM lead
WHERE primary_email IS NULL;

DELETE FROM lead
WHERE id IN (SELECT id FROM lead_purge);

--DROP TABLE raw_source;


--CREATE INDEX ix_activity_querystring ON activity USING gin ((attributes -> 'querystring'));
--CREATE INDEX spelers_name_special_idx ON spelers (name text_pattern_ops);

/*
explain analyze SELECT lead.id AS lead_id, account.id AS account_id, row_number() OVER (PARTITION BY lead.id ORDER BY char_length(account.domain_regex) DESC, account.id ASC) AS rnk
FROM lead JOIN account ON lead.email ~ account.domain_regex
WHERE lead.account_id IS NULL;

explain analyze SELECT lead.id AS lead_id, account.id AS account_id, row_number() OVER (PARTITION BY lead.id ORDER BY char_length(account.domain_regex) DESC, account.id ASC) AS rnk
FROM lead JOIN account ON lead.email like concat('%', account.domain_regex, '%')
WHERE lead.account_id IS NULL;

explain analyze SELECT lead.id AS lead_id, account.id AS account_id, row_number() OVER (PARTITION BY lead.id ORDER BY char_length(account.domain_regex) DESC, account.id ASC) AS rnk
FROM lead JOIN account ON lead.email like concat(account.domain_regex, '%')
WHERE lead.account_id IS NULL;

*/
