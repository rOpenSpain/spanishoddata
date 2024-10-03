CREATE VIEW os_csv_clean AS SELECT
    fecha AS date,
    CAST(zona_residencia AS RESID_ZONES_ENUM) AS id_residence,
    CAST(zona_pernoctacion AS OVERNIGHT_ZONES_ENUM) AS id_overnight_stay,
    personas AS n_persons,
    CAST(year AS INTEGER) AS year,
    CAST(month AS INTEGER) AS month,
    CAST(day AS INTEGER) AS day
FROM os_csv_raw;
