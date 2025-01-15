-- use powershell terminal
duckdb

ATTACH 'data/lnrs.duckdb' AS lnrs;
INSTALL HTTPFS;
LOAD HTTPFS;

CREATE OR REPLACE TABLE lnrs_area_measures_priorities_tbl AS
SELECT * 
FROM read_csv(
'https://opendata.westofengland-ca.gov.uk/api/explore/v2.1/catalog/datasets/area-measures-tbl/exports/csv?lang=en&timezone=Europe%2FLondon&use_labels=false&delimiter=%2C'
);

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
(SELECT priority_id
    , biodiversity_priority
    , simplified_biodiversity_priority
    , theme
FROM lnrs_area_measures_priorities_tbl
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

SELECT COLUMNS(c -> c LIKE '%id')
FROM lnrs_area_measures_priorities_tbl
GROUP BY ALL;

.tables

.quit