CREATE OR REPLACE VIEW nt_csv_clean AS SELECT
    fecha,
    CAST(zona_pernoctacion AS ZONES_ENUM) AS zona_pernoctacion,
    CAST(CASE edad
        WHEN 'NA' THEN NULL
        WHEN '0-25' THEN '0-25'
        WHEN '25-45' THEN '25-45'
        WHEN '45-65' THEN '45-65'
        WHEN '65-100' THEN '65-100'
        ELSE NULL
        END AS AGE_ENUM)
        AS edad,
    CAST (CASE sexo
        WHEN 'NA' THEN NULL
        ELSE sexo
        END AS SEX_ENUM)
        AS sexo,
    CAST(numero_viajes AS N_TRIPS_ENUM) AS numero_viajes,
    personas,
    CAST(year AS INTEGER) AS ano,
    CAST(month AS INTEGER) AS mes,
    CAST(day AS INTEGER) AS dia
FROM nt_csv_raw;
