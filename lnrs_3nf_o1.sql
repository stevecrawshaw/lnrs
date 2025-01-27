-- bash
-- https://www.perplexity.ai/search/system-you-are-an-advanced-ana-FmQBBisrSx._7unn8VwhmQ
-- break down the big table into smaller tables into 3NF
-- The aim is to develop a neater process to edit and update source data, ie. add a new measure type, or stakeholder
-- and recreate


rm data/lnrs_3nf_o1.duckdb

duckdb
ATTACH 'data/lnrs_3nf_o1.duckdb' AS lnrs;
--INSTALL HTTPFS;
-- LOAD HTTPFS;

USE lnrs;

.tables

CREATE OR REPLACE TABLE source_table AS
SELECT * 
FROM read_parquet('data/area-measures-tbl.parquet');

--            CREATE TABLES                                     --
------------------------------------------------------------------
-- 1) MEASURE
------------------------------------------------------------------
CREATE OR REPLACE TABLE measure (
    measure_id  INTEGER NOT NULL PRIMARY KEY,
    measure     VARCHAR,
    other_priorities_delivered VARCHAR,
    core_supplementary        VARCHAR,
    mapped_unmapped           VARCHAR,
    relevant_map_layer        VARCHAR,
    link_to_further_guidance  VARCHAR
);

------------------------------------------------------------------
-- 2) MEASURE_TYPE
------------------------------------------------------------------
CREATE OR REPLACE SEQUENCE seq_measure_type_id START 1;

CREATE OR REPLACE TABLE measure_type (
    measure_type_id INTEGER NOT NULL PRIMARY KEY DEFAULT nextval('seq_measure_type_id'),
    measure_type    VARCHAR
);

------------------------------------------------------------------
-- 3) STAKEHOLDER
------------------------------------------------------------------
CREATE OR REPLACE SEQUENCE seq_stakeholder_id START 1;

CREATE OR REPLACE TABLE stakeholder (
    stakeholder_id INTEGER NOT NULL PRIMARY KEY DEFAULT nextval('seq_stakeholder_id'),
    stakeholder    VARCHAR
);

------------------------------------------------------------------
-- 4) MEASURE_HAS_TYPE (bridge)
------------------------------------------------------------------
CREATE OR REPLACE TABLE measure_has_type (
    measure_id      INTEGER NOT NULL,
    measure_type_id INTEGER NOT NULL,
    PRIMARY KEY (measure_id, measure_type_id),
    FOREIGN KEY (measure_id)      REFERENCES measure(measure_id),
    FOREIGN KEY (measure_type_id) REFERENCES measure_type(measure_type_id)
);

------------------------------------------------------------------
-- 5) MEASURE_HAS_STAKEHOLDER (bridge)
------------------------------------------------------------------
CREATE OR REPLACE TABLE measure_has_stakeholder (
    measure_id     INTEGER NOT NULL,
    stakeholder_id INTEGER NOT NULL,
    PRIMARY KEY (measure_id, stakeholder_id),
    FOREIGN KEY (measure_id)     REFERENCES measure(measure_id),
    FOREIGN KEY (stakeholder_id) REFERENCES stakeholder(stakeholder_id)
);

------------------------------------------------------------------
-- 6) AREA
------------------------------------------------------------------
CREATE OR REPLACE TABLE area (
    area_id          INTEGER NOT NULL PRIMARY KEY,
    area_name        VARCHAR,
    area_description VARCHAR,
    area_link        VARCHAR,
    bng_hab_mgt      VARCHAR,
    bng_hab_creation VARCHAR,
    local_funding_schemes VARCHAR
);

------------------------------------------------------------------
-- 7) PRIORITY
------------------------------------------------------------------
CREATE OR REPLACE TABLE priority (
    priority_id INTEGER NOT NULL PRIMARY KEY,
    biodiversity_priority           VARCHAR,
    simplified_biodiversity_priority VARCHAR,
    theme                           VARCHAR
);

------------------------------------------------------------------
-- 8) GRANT_TABLE (renamed to avoid reserved keyword)
------------------------------------------------------------------
CREATE OR REPLACE TABLE grant_table (
    grant_id              VARCHAR NOT NULL PRIMARY KEY,
    grant_name            VARCHAR,
    grant_scheme          VARCHAR,
    url                   VARCHAR,
    summary_wrapped       VARCHAR
);

------------------------------------------------------------------
-- 9) MEASURE_AREA_PRIORITY
------------------------------------------------------------------
CREATE OR REPLACE TABLE measure_area_priority (
    measure_id  INTEGER NOT NULL,
    area_id     INTEGER NOT NULL,
    priority_id INTEGER NOT NULL,
    PRIMARY KEY (measure_id, area_id, priority_id),
    FOREIGN KEY (measure_id)  REFERENCES measure(measure_id),
    FOREIGN KEY (area_id)     REFERENCES area(area_id),
    FOREIGN KEY (priority_id) REFERENCES priority(priority_id)
);

------------------------------------------------------------------
-- 10) MEASURE_AREA_PRIORITY_GRANT
------------------------------------------------------------------
CREATE OR REPLACE TABLE measure_area_priority_grant (
    measure_id  INTEGER NOT NULL,
    area_id     INTEGER NOT NULL,
    priority_id INTEGER NOT NULL,
    grant_id    VARCHAR NOT NULL,
    PRIMARY KEY (measure_id, area_id, priority_id, grant_id),
    FOREIGN KEY (measure_id, area_id, priority_id)
        REFERENCES measure_area_priority (measure_id, area_id, priority_id),
    FOREIGN KEY (grant_id) REFERENCES grant_table(grant_id)
);


.tables
----------------------------------------------------------------------------
--                   INSERT STATEMENTS                                    --
----------------------------------------------------------------------------

-- 1) Insert into measure

INSERT INTO measure (
    measure_id,
    measure,
    other_priorities_delivered,
    core_supplementary,
    mapped_unmapped,
    relevant_map_layer,
    link_to_further_guidance
)
SELECT DISTINCT
    measure_id,
    measure,
    other_priorities_delivered,
    core_supplementary,
    mapped_unmapped,
    relevant_map_layer,
    link_to_further_guidance
FROM source_table
WHERE measure_id IS NOT NULL;

-- area insert

INSERT INTO area (
    area_id,
    area_name,
    area_description,
    area_link,
    bng_hab_mgt,
    bng_hab_creation,
    local_funding_schemes
)
SELECT DISTINCT
    area_id,
    area_name,
    area_description,
    area_link,
    bng_hab_mgt,
    bng_hab_creation,
    local_funding_schemes
FROM source_table
WHERE area_id IS NOT NULL;

-- priority insert

INSERT INTO priority (
    priority_id,
    biodiversity_priority,
    simplified_biodiversity_priority,
    theme
)
SELECT DISTINCT
    priority_id,
    biodiversity_priority,
    simplified_biodiversity_priority,
    theme
FROM source_table
WHERE priority_id IS NOT NULL;

-- grant insert

INSERT INTO grant_table (
    grant_id,
    grant_name,
    grant_scheme,
    "url",
    summary_wrapped
)
SELECT DISTINCT
    grant_id,
    grant_name,
    grant_scheme,
    "url",
    summary_wrapped
FROM source_table
WHERE grant_id IS NOT NULL;

-- measure types

INSERT INTO measure_type BY NAME 
    (SELECT DISTINCT
    measure_type
FROM source_table);
    
-- stakeholder insert

INSERT INTO stakeholder BY NAME
    (SELECT DISTINCT
    stakeholder
FROM source_table);

FROM stakeholder;

-- has measure type insert

INSERT INTO measure_has_type (measure_id, measure_type_id)
SELECT DISTINCT
    s.measure_id,
    mt.measure_type_id
FROM source_table s
JOIN measure m
    ON s.measure_id = m.measure_id
JOIN measure_type mt
    ON s.measure_type = mt.measure_type
WHERE s.measure_id IS NOT NULL
  AND s.measure_type IS NOT NULL;

-- measure has stakeholder insert

INSERT INTO measure_has_stakeholder (measure_id, stakeholder_id)
SELECT DISTINCT
    s.measure_id,
    stkh.stakeholder_id
FROM source_table s
JOIN measure m
    ON s.measure_id = m.measure_id
JOIN stakeholder stkh
    ON s.stakeholder = stkh.stakeholder
WHERE s.measure_id IS NOT NULL
  AND s.stakeholder IS NOT NULL;

-- measure_area_priority insert

-- Each row in source_table links a specific 
-- (measure_id, area_id, priority_id). We capture it here, ignoring duplicates:

INSERT INTO measure_area_priority (
    measure_id,
    area_id,
    priority_id
)
SELECT DISTINCT
    measure_id,
    area_id,
    priority_id
FROM source_table
WHERE measure_id IS NOT NULL
  AND area_id IS NOT NULL
  AND priority_id IS NOT NULL;

-- 6) Insert into measure_area_priority_grant (bridge for grants)
-- Some measure–area–priority combos have an associated grant_id. Insert them here:

INSERT INTO measure_area_priority_grant (
    measure_id,
    area_id,
    priority_id,
    grant_id
)
SELECT DISTINCT
    measure_id,
    area_id,
    priority_id,
    grant_id
FROM source_table
WHERE measure_id IS NOT NULL
  AND area_id IS NOT NULL
  AND priority_id IS NOT NULL
  AND grant_id IS NOT NULL;

-- Recreate source_table with a Single  Query
-- If you wish to see all of the columns in a single result set (mirroring source_table), 
-- you can do so with the following join query. 
-- The many-to-many relationship to grants is handled by left-joining on the 
-- measure_area_priority_grant table, as some measure–area–priority rows may not have an associated grant.

CREATE OR REPLACE VIEW source_table_recreated_vw AS
SELECT
    /* Measures */
    m.measure_id,
    m.measure,
    m.other_priorities_delivered,
    m.core_supplementary,
    m.mapped_unmapped,
    m.relevant_map_layer,
    m.link_to_further_guidance,

    /* Measure Types (one row per measure_type if multiple exist) */
    mt.measure_type,

    /* Stakeholders (one row per stakeholder if multiple exist) */
    stkh.stakeholder,

    /* Area */
    map.area_id,
    a.area_name,
    a.area_description,
    a.area_link,
    a.bng_hab_mgt,
    a.bng_hab_creation,
    a.local_funding_schemes,

    /* Priority */
    map.priority_id,
    p.biodiversity_priority,
    p.simplified_biodiversity_priority,
    p.theme,

    /* Grant */
    mag.grant_id,
    g.grant_name,
    g.grant_scheme,
    g.summary_wrapped,
    g.url

FROM measure_area_priority AS map
JOIN measure AS m
  ON map.measure_id = m.measure_id
JOIN area AS a
  ON map.area_id = a.area_id
JOIN priority AS p
  ON map.priority_id = p.priority_id

-- Many-to-many from measure -> measure_type
LEFT JOIN measure_has_type AS mht
       ON m.measure_id = mht.measure_id
LEFT JOIN measure_type AS mt
       ON mht.measure_type_id = mt.measure_type_id

-- Many-to-many from measure -> stakeholder
LEFT JOIN measure_has_stakeholder AS mhs
       ON m.measure_id = mhs.measure_id
LEFT JOIN stakeholder AS stkh
       ON mhs.stakeholder_id = stkh.stakeholder_id

-- Potential multiple grants per measure–area–priority
LEFT JOIN measure_area_priority_grant AS mag
       ON  map.measure_id  = mag.measure_id
       AND map.area_id     = mag.area_id
       AND map.priority_id = mag.priority_id
LEFT JOIN grant_table AS g
       ON mag.grant_id = g.grant_id;


DESCRIBE FROM source_table;

-- testing why there are fewer rows in the recreated table
-- compared to the source table
-- it is because the recreated table elides rows where a grant ID
-- is NULL but all other  unique identifiers exist

-- needs to be tested in the TEST app
-- also lets try removing all unused fields from the dataset as it is only used for the app

CREATE OR REPLACE VIEW source_table_distinct_vw AS
SELECT measure_id, priority_id, grant_id, measure_type, stakeholder 
FROM source_table st
-- INNER JOIN source_table_recreated str
-- USING (measure_id, priority_id, area_id)
WHERE st.area_id = 15;


CREATE OR REPLACE VIEW source_table_recreated_distinct_vw AS
SELECT measure_id, priority_id, grant_id, measure_type, stakeholder 
FROM source_table_recreated_vw str
-- INNER JOIN source_table_recreated str
-- USING (measure_id, priority_id, area_id)
WHERE str.area_id = 15;

SELECT * FROM source_table_distinct_vw;

(   SELECT * FROM source_table_distinct_vw
    EXCEPT
    SELECT * FROM source_table_recreated_distinct_vw)  
UNION ALL
(   SELECT * FROM source_table_recreated_distinct_vw
    EXCEPT
    SELECT * FROM source_table_distinct_vw); 

-- a revised version of the area-measures-tbl which keeps only the fields
-- necessary for the app

CREATE OR REPLACE VIEW apmg_slim_vw AS
SELECT
    core_supplementary
    , measure_type
    , stakeholder
    , area_name
    , grant_id
    , priority_id
    , biodiversity_priority
    , measure
    , measure_id
    , link_to_further_guidance
    , grant_name
    , "url"
FROM source_table_recreated_vw;


-- try JSON as CSV is outputting invalid encoding of non alphanumeric characters
COPY apmg_slim_vw TO 'data/apmg_slim_ods.json' (ARRAY true);

-- Now we need a process to update (edit) the values in the individual tables
-- and then update the source_table_recreated view

----------------------------------------------------------------
-- CRUD OPEARATION ON THE TABLES
----------------------------------------------------------------

-- 1. Update Operations
-- a. Update a record in the "measure" table
UPDATE measure
SET measure = 'New Measure Description'
WHERE measure_id = 1;

-- b. Update a record in the "measure_type" table

UPDATE measure_type
SET measure_type = 'New Type'
WHERE measure_type_id = 1;


-- c. Update a record in the "stakeholder" table

UPDATE stakeholder
SET stakeholder = 'New Stakeholder Name'
WHERE stakeholder_id = 1;
-- d. Update a record in the "grant_table" table

UPDATE grant_table
SET grant_name = 'New Grant Name'
WHERE grant_id = 'GRANT123';
-- e. Update a record in the "priority" table

UPDATE priority
SET biodiversity_priority = 'New Priority'
WHERE priority_id = 1;
-- f. Update a record in the "area" table

UPDATE area
SET area_name = 'New Area Name'
WHERE area_id = 1;
-- 2. Create Operations
-- a. Add a record to the "measure" table

-- generate a new incrnemnted measure_id
CREATE MACRO max_meas() AS (SELECT MAX(measure_id) + 1 as max_measure_id FROM measure);
SELECT max_meas();

INSERT INTO measure (measure_id, measure, other_priorities_delivered, core_supplementary, mapped_unmapped, relevant_map_layer, link_to_further_guidance)
VALUES (max_meas(), 'New Measure', 'Priority A', 'Core', 'Mapped', 'Layer 1', 'https://example.com');


SELECT * FROM measure WHERE measure_id >= 780;
-- b. Add a record to the "area" table

INSERT INTO area (area_id, area_name, area_description, area_link, bng_hab_mgt, bng_hab_creation, local_funding_schemes)
VALUES (80, 'New Area', 'Area Description', 'https://example.com', 'Management Plan', 'Creation Plan', 'Scheme A');
-- c. Add a record to the "measure_type" table

INSERT INTO measure_type (measure_type)
VALUES ('New Type');
-- d. Add a record to the "stakeholder" table

INSERT INTO stakeholder (stakeholder)
VALUES ('New Stakeholder');
-- e. Add a record to the "grant_table" table

INSERT INTO grant_table (grant_id, grant_name, grant_scheme, url, summary_wrapped)
VALUES ('GRANT456', 'New Grant', 'Scheme X', 'https://example.com', 'Grant Summary');
-- f. Add a record to the "priority" table

INSERT INTO priority (priority_id, biodiversity_priority, simplified_biodiversity_priority, theme)
VALUES (788, 'New Priority', 'Simplified Priority', 'Theme A');
-- 3. Delete Operations
-- a. Delete a record from the "measure" table
-------------------------------------------------------------------------
-- Need to delete the rows with the measure_id from all linked tables first

DELETE FROM measure_has_type 
WHERE measure_type_id = (SELECT measure_type_id FROM measure_has_type
WHERE measure_id = 1);

DELETE FROM measure_area_priority_grant
WHERE measure_id = 1;

DELETE FROM measure_area_priority
WHERE measure_id = 1;

-- stakeholder delete
DELETE FROM measure_has_stakeholder
WHERE stakeholder_id = 1;

DELETE FROM stakeholder
WHERE stakeholder_id = 1;

-- d. Delete a record from the "grant_table" table

DELETE FROM grant_table
WHERE grant_id = 'GRANT123';
-- e. Delete a record from the "priority" table
-------------------------------------------------------------------------

DELETE FROM measure_area_priority_grant
WHERE priority_id = 1;

DELETE FROM measure_area_priority
WHERE priority_id = 1;

DELETE FROM priority
WHERE priority_id = 1;
-- f. Delete a record from the "area" table

DELETE FROM measure_area_priority_grant
WHERE area_id = 1;

DELETE FROM measure_area_priority
WHERE area_id = 1;

DELETE FROM area
WHERE area_id = 1;


.help

.quit