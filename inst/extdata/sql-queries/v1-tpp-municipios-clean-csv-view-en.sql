CREATE VIEW tpp_csv_clean AS SELECT
    fecha AS date,
    CAST(CASE distrito
    /* fixing the issue with different codes in different data files
    for details see issue documented here:
    - https://github.com/e-kotov/mitma-data-issues/issues/8
    - http://www.ekotov.pro/mitma-data-issues/issues/011-v1-tpp-mismatch-zone-ids-in-table-and-spatial-data.html
    */
        WHEN '04902' THEN '0490201'
        WHEN '28006' THEN '2800601'
        WHEN '28106' THEN '2810601'
        WHEN '28123' THEN '2812301'
        WHEN '28127' THEN '2812701'
    /* double check if this is the way, the problem may be somewhere else */
        WHEN '0105901' THEN '01059'
        ELSE distrito
    END AS ZONES_ENUM) AS id,
    CAST(numero_viajes AS N_TRIPS_ENUM) AS n_trips,
    personas AS n_persons,
    CAST(year AS INTEGER) AS year,
    CAST(month AS INTEGER) AS month,
    CAST(day AS INTEGER) AS day
FROM tpp_csv_raw;
