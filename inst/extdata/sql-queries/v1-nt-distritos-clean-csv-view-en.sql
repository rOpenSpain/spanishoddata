CREATE OR REPLACE VIEW nt_csv_clean AS SELECT
    fecha AS date,
    CAST (CASE distrito
        WHEN 'externo' THEN 'external'
        ELSE distrito
        END AS ZONES_ENUM)
        AS id,
    CAST(numero_viajes AS N_TRIPS_ENUM) AS n_trips,
    personas AS n_persons,
    CAST(year AS INTEGER) AS year,
    CAST(month AS INTEGER) AS month,
    CAST(day AS INTEGER) AS day
FROM nt_csv_raw;
