duckdb
COPY
(SELECT measure, area_name
FROM 
read_csv('https://opendata.westofengland-ca.gov.uk/api/explore/v2.1/catalog/datasets/apmg_slim_ods/exports/csv?lang=en&timezone=Europe%2FLondon&use_labels=false&delimiter=%2C')
GROUP BY all
ORDER BY area_name, measure)
TO 'data/areas_measures_hannah.parquet';