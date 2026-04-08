CREATE OR REPLACE VIEW nt_csv_clean AS SELECT
    fecha,
    CAST (CASE distrito WHEN 'NA' THEN NULL ELSE distrito END AS ZONES_ENUM) AS distrito,
    CAST (CASE numero_viajes WHEN 'NA' THEN NULL ELSE numero_viajes END AS N_TRIPS_ENUM) AS numero_viajes,
    personas,
    CAST(year AS INTEGER) AS ano,
    CAST(month AS INTEGER) AS mes,
    CAST(day AS INTEGER) AS dia
FROM nt_csv_raw;
