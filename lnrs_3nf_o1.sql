-- bash
duckdb

ATTACH 'data/lnrs_3nf_o1.duckdb' AS lnrs;
INSTALL HTTPFS;
LOAD HTTPFS;

USE lnrs;

CREATE OR REPLACE TABLE source_table AS
SELECT * 
FROM read_parquet('data/area-measures-tbl.parquet');

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
CREATE SEQUENCE seq_measure_type_id START 1;

CREATE OR REPLACE TABLE measure_type (
    measure_type_id INTEGER NOT NULL PRIMARY KEY DEFAULT nextval('seq_measure_type_id'),
    measure_type    VARCHAR
);

------------------------------------------------------------------
-- 3) STAKEHOLDER
------------------------------------------------------------------
CREATE SEQUENCE seq_stakeholder_id START 1;

CREATE OR REPLACE TABLE stakeholder (
    stakeholder_id INTEGER NOT NULL PRIMARY KEY,
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
    bng_hab_creation VARCHAR
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
    summary_wrapped       VARCHAR,
    local_funding_schemes VARCHAR
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
-- Step 3: Query source_table to Populate the Component Tables

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
    bng_hab_creation
)
SELECT DISTINCT
    area_id,
    area_name,
    area_description,
    area_link,
    bng_hab_mgt,
    bng_hab_creation
FROM source_table
WHERE area_id IS NOT NULL;

FROM area;

-- measure types

INSERT INTO measure_type VALUES (
    nextval('seq_measure_type_id'), 
    (SELECT DISTINCT
    measure_type
FROM source_table)
);
    



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


-- measure_area_priority insert
-- Each row in source_table links a specific (measure_id, area_id, priority_id). We capture it here, ignoring duplicates:

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

-- Step 4: Recreate source_table with a Single SQL Query
-- If you wish to see all of the columns in a single result set (mirroring source_table), 
-- you can do so with the following join query. 
-- The many-to-many relationship to grants is handled by left-joining on the 
-- measure_area_priority_grant table, as some measure–area–priority rows may not have an associated grant.

SELECT
    map.measure_id,
    map.priority_id,
    m.measure,
    m.other_priorities_delivered,
    m.core_supplementary,
    m.mapped_unmapped,
    m.measure_type,
    m.stakeholder,
    m.relevant_map_layer,
    m.link_to_further_guidance,

    -- Area columns (including bng_hab_xxx moved here)
    map.area_id,
    a.area_name,
    a.area_description,
    a.area_link,
    a.bng_hab_mgt,
    a.bng_hab_creation,

    -- Priority columns
    p.theme,
    p.biodiversity_priority,
    p.simplified_biodiversity_priority,

    -- Grant columns
    mag.grant_id,
    g.grant_name,
    g.grant_scheme,
    g.summary_wrapped,
    g.url,
    g.local_funding_schemes

FROM measure_area_priority AS map

-- Join to measure, area, priority
JOIN measure   AS m ON map.measure_id = m.measure_id
JOIN area      AS a ON map.area_id    = a.area_id
JOIN priority  AS p ON map.priority_id = p.priority_id

-- Left-join to measure_area_priority_grant
LEFT JOIN measure_area_priority_grant AS mag
       ON  map.measure_id  = mag.measure_id
       AND map.area_id     = mag.area_id
       AND map.priority_id = mag.priority_id

-- Left-join to grant
LEFT JOIN grant_table AS g
       ON mag.grant_id = g.grant_id
;

-- This query will yield a result set that looks structurally the same as the original source_table.
-- You can optionally persist this result into a new table named, for example, source_table_rebuilt:

CREATE OR REPLACE TABLE source_table_rebuilt AS
SELECT
    ...
FROM measure_area_priority AS map
...
LEFT JOIN grant_table AS g
       ON mag.grant_id = g.grant_id;


.tables

.quit