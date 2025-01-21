-- bash
duckdb

ATTACH 'data/lnrs_3nf.duckdb' AS lnrs;
INSTALL HTTPFS;
LOAD HTTPFS;

USE lnrs;

CREATE OR REPLACE TABLE source_table AS
SELECT * 
FROM read_parquet('data/area-measures-tbl.parquet');

-- Create normalized tables -----------------------------------------------------
CREATE OR REPLACE TABLE Measures (
    measure_id INT PRIMARY KEY,
    priority_id INT,
    measure TEXT,
    measure_type TEXT,
    stakeholder TEXT,
    core_supplementary TEXT,
    mapped_unmapped TEXT,
    relevant_map_layer TEXT,
    link_to_further_guidance TEXT
);

CREATE OR REPLACE TABLE Areas (
    area_id INT PRIMARY KEY,
    area_name TEXT,
    area_description TEXT,
    area_link TEXT
);

CREATE OR REPLACE TABLE Priorities (
    priority_id INT PRIMARY KEY,
    biodiversity_priority TEXT,
    simplified_biodiversity_priority TEXT
);

CREATE OR REPLACE TABLE Grants (
    grant_id TEXT PRIMARY KEY,
    grant_name TEXT,
    grant_scheme TEXT,
    "url" TEXT,
    summary_wrapped TEXT
);

CREATE OR REPLACE TABLE Themes (
    theme_id INT PRIMARY KEY,
    theme TEXT,
    biodiversity_priority TEXT,
    simplified_biodiversity_priority TEXT
);

CREATE OR REPLACE TABLE Measure_Area_Priority (
    measure_id INT,
    area_id INT,
    priority_id INT,
    theme_id INT,
    grant_id TEXT,
    FOREIGN KEY (measure_id) REFERENCES Measures(measure_id),
    FOREIGN KEY (area_id) REFERENCES Areas(area_id),
    FOREIGN KEY (priority_id) REFERENCES Priorities(priority_id),
    FOREIGN KEY (theme_id) REFERENCES Themes(theme_id),
    FOREIGN KEY (grant_id) REFERENCES Grants(grant_id),
    PRIMARY KEY (measure_id, area_id, priority_id)
);

-- create priorities dependencies

CREATE OR REPLACE TABLE Priority_Dependencies (
    priority_id INT,
    dependent_priority_id INT,
    FOREIGN KEY (priority_id) REFERENCES Priorities(priority_id),
    FOREIGN KEY (dependent_priority_id) REFERENCES Priorities(priority_id),
    PRIMARY KEY (priority_id, dependent_priority_id)
);

-- create habitat management

CREATE OR REPLACE TABLE Habitat_Management (
    area_id INT,
    habitat_type TEXT,
    management_type TEXT CHECK (management_type IN ('creation', 'management')),
    FOREIGN KEY (area_id) REFERENCES Areas(area_id),
    PRIMARY KEY (area_id, habitat_type, management_type)
);

.tables

-- extract data from the source table to populate the 3NF tables


-- extract Measures data
-- a measure can have multiple stakeholders and types
INSERT INTO Measures
SELECT
    measure_id
    , priority_id
    , measure
--    , stakeholder
    , core_supplementary
    , mapped_unmapped
    , relevant_map_layer
    , link_to_further_guidance
FROM source_table
GROUP BY ALL;

-- extract Areas data

SELECT
    area_id
    , area_name
    , area_description
    , area_link
FROM source_table
GROUP BY ALL;

-- extract Priorities data

SELECT
    priority_id
    , core_supplementary
    , mapped_unmapped
FROM source_table
GROUP BY ALL;

-- extract Grants data

SELECT
    grant_id
    , grant_name
    , grant_scheme
    , "url"
    , summary_wrapped
FROM source_table
GROUP BY ALL;

-- extract themes data

WITH theme_data AS (
    SELECT DISTINCT
        theme,
        biodiversity_priority,
        simplified_biodiversity_priority
    FROM source_table
    WHERE theme IS NOT NULL
),
numbered_themes AS (
    SELECT 
        ROW_NUMBER() OVER (ORDER BY theme) as theme_id,
        theme,
        biodiversity_priority,
        simplified_biodiversity_priority
    FROM theme_data
)
INSERT INTO Themes
SELECT * FROM numbered_themes;

FROM Themes;


-- Extract Priority Dependencies
INSERT INTO Priority_Dependencies
SELECT * FROM 
(WITH RECURSIVE split_priorities AS (
    SELECT 
        priority_id,
        unnest(string_to_array(other_priorities_delivered, ';')) AS dependent_priority_id
    FROM source_table
)
SELECT DISTINCT
    priority_id,
    CAST(dependent_priority_id AS INTEGER) AS dependent_priority_id
FROM split_priorities
WHERE dependent_priority_id IS NOT NULL);

-- Extract Habitat Management
SELECT 
    area_id,
    unnest(string_to_array(trim(both ':' from bng_hab_mgt), ',')) as habitat_type,
    'management' as management_type
FROM source_table
WHERE bng_hab_mgt IS NOT NULL

UNION ALL

SELECT 
    area_id,
    unnest(string_to_array(trim(both ':' from bng_hab_creation), ',')) as habitat_type,
    'creation' as management_type
FROM source_table
WHERE bng_hab_creation IS NOT NULL;

FROM Priority_Dependencies;

















CREATE OR REPLACE TABLE lnrs_area_measures_priorities_tbl AS
SELECT * 
-- FROM read_csv(
-- 'https://opendata.westofengland-ca.gov.uk/api/explore/v2.1/catalog/datasets/area-measures-tbl/exports/csv?lang=en&timezone=Europe%2FLondon&use_labels=false&delimiter=%2C')
FROM read_parquet('https://opendata.westofengland-ca.gov.uk/api/explore/v2.1/catalog/datasets/area-measures-tbl/exports/parquet?lang=en&timezone=Europe%2FLondon')
;

FROM lnrs_area_measures_priorities_tbl
GROUP BY ALL;



DESCRIBE lnrs_area_measures_priorities_tbl;

CREATE OR REPLACE VIEW areas_vw AS
(SELECT
    area_id
    , area_name
    , area_description
    , area_link
    , local_funding_schemes
    , bng_hab_mgt
    , bng_hab_creation
FROM lnrs_area_measures_priorities_tbl
GROUP BY ALL);

CREATE OR REPLACE VIEW priorities_vw AS
(SELECT * 
FROM read_csv("https://opendata.westofengland-ca.gov.uk/api/explore/v2.1/catalog/datasets/priorities-grouped-tbl/exports/csv?lang=en&timezone=Europe%2FLondon&use_labels=false&delimiter=%2C")
GROUP BY ALL
ORDER BY priority_id);

CREATE OR REPLACE VIEW measures_vw AS
(SELECT * FROM 
read_csv('https://opendata.westofengland-ca.gov.uk/api/explore/v2.1/catalog/datasets/lnrs-measures/exports/csv?lang=en&timezone=Europe%2FLondon&use_labels=false&delimiter=%2C')
ORDER BY measure_id);

CREATE OR REPLACE VIEW grants_vw AS
(SELECT * FROM
read_csv('https://opendata.westofengland-ca.gov.uk/api/explore/v2.1/catalog/datasets/lnrs-all-grants-tbl/exports/csv?lang=en&timezone=Europe%2FLondon&use_labels=false&delimiter=%2C')
ORDER BY grant_id);

CREATE OR REPLACE VIEW lookups_vw AS
(SELECT COLUMNS(c -> c LIKE '%id')
FROM lnrs_area_measures_priorities_tbl);

-- bng columns relate to areas 1:1
SELECT COLUMNS("bng*"), area_id
FROM lnrs_area_measures_priorities_tbl
GROUP BY ALL
HAVING area_id = 1;

CREATE OR REPLACE TABLE lnrs_funding_schemes_tbl AS
SELECT * FROM
read_csv(
'https://opendata.westofengland-ca.gov.uk/api/explore/v2.1/catalog/datasets/lnrs-area-funding-schemes-tbl/exports/csv?lang=en&timezone=Europe%2FLondon&use_labels=false&delimiter=%2C');

DESCRIBE (SELECT * FROM lookups_vw
LEFT JOIN areas_vw
ON lookups_vw.area_id = areas_vw.area_id
LEFT JOIN priorities_vw
ON lookups_vw.priority_id = priorities_vw.priority_id
LEFT JOIN measures_vw
ON lookups_vw.measure_id = measures_vw.measure_id 
LEFT JOIN grants_vw
ON lookups_vw.grant_id = grants_vw.grant_id
WHERE measures_vw.stakeholder = 'Local Government'
GROUP BY ALL);


SELECT * FROM measures_vw;

.tables

.quit