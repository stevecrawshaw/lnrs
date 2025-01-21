-- bash
duckdb

ATTACH 'data/lnrs.duckdb' AS lnrs;
INSTALL HTTPFS;
LOAD HTTPFS;

USE lnrs;

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