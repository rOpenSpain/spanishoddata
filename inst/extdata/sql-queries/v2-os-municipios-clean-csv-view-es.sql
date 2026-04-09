CREATE OR REPLACE VIEW os_csv_clean AS SELECT
    fecha,
    CAST (CASE zona_residencia WHEN 'NA' THEN NULL ELSE zona_residencia END AS RESID_ZONES_ENUM) AS zona_residencia,
    CAST (CASE zona_pernoctacion WHEN 'NA' THEN NULL ELSE zona_pernoctacion END AS OVERNIGHT_ZONES_ENUM) AS zona_pernoctacion,
    personas,
    CAST(year AS INTEGER) AS ano,
    CAST(month AS INTEGER) AS mes,
    CAST(day AS INTEGER) AS dia
FROM os_csv_raw;
