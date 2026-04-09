CREATE OR REPLACE VIEW nt_csv_clean AS SELECT
    fecha AS date,
    CAST (CASE zona_pernoctacion WHEN 'NA' THEN NULL ELSE zona_pernoctacion END AS ZONES_ENUM) AS id,
    CAST(CASE edad
        WHEN 'NA' THEN NULL
        WHEN '0-25' THEN '0-25'
        WHEN '25-45' THEN '25-45'
        WHEN '45-65' THEN '45-65'
        WHEN '65-100' THEN '65-100'
        ELSE NULL
        END AS AGE_ENUM)
        AS age,
    CAST(CASE sexo
        WHEN 'NA' THEN NULL
        WHEN 'mujer' THEN 'female'
        WHEN 'hombre' THEN 'male'
        END AS SEX_ENUM)
        AS sex,
    CAST (CASE numero_viajes WHEN 'NA' THEN NULL ELSE numero_viajes END AS N_TRIPS_ENUM) AS n_trips,
    personas AS n_persons,
    CAST(year AS INTEGER) AS year,
    CAST(month AS INTEGER) AS month,
    CAST(day AS INTEGER) AS day
FROM nt_csv_raw;
