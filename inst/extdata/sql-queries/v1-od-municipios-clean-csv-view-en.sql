CREATE VIEW od_csv_clean AS SELECT
    fecha AS date,
    CAST(origen AS ZONES_ENUM) AS id_origin,
    CAST(destino AS ZONES_ENUM) AS id_destination,
    periodo AS time_slot,
    CAST(distancia AS DISTANCE_ENUM) AS distance,
    viajes AS n_trips,
    viajes_km AS trips_total_length_km,
    CAST(year AS INTEGER) AS year,
    CAST(month AS INTEGER) AS month,
    CAST(day AS INTEGER) AS day
FROM od_csv_raw;
