CREATE OR REPLACE VIEW os_csv_clean AS SELECT
    fecha,
    CAST(zona_residencia AS RESID_ZONES_ENUM) AS zona_residencia,
    CAST(zona_pernoctacion AS OVERNIGHT_ZONES_ENUM) AS zona_pernoctacion,
    personas,
    CAST(year AS INTEGER) AS ano,
    CAST(month AS INTEGER) AS mes,
    CAST(day AS INTEGER) AS dia
FROM os_csv_raw;
