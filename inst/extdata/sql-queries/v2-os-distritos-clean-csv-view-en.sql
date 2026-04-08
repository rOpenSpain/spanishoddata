CREATE OR REPLACE VIEW os_csv_clean AS SELECT
    fecha AS date,
    CAST (CASE zona_residencia WHEN 'NA' THEN NULL ELSE zona_residencia END AS RESID_ZONES_ENUM) AS id_residence,
    CAST (CASE zona_pernoctacion WHEN 'NA' THEN NULL ELSE zona_pernoctacion END AS OVERNIGHT_ZONES_ENUM) AS id_overnight_stay,
    personas AS n_persons,
    CAST(year AS INTEGER) AS year,
    CAST(month AS INTEGER) AS month,
    CAST(day AS INTEGER) AS day
FROM os_csv_raw;
