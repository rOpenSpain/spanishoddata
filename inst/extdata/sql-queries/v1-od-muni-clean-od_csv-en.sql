CREATE VIEW od_csv_clean AS SELECT
    fecha AS full_date,
    CAST(origen AS ZONES_ENUM) AS id_origin,
    CAST(destino AS ZONES_ENUM) AS id_destination,
    periodo AS time_slot,
    CAST(distancia AS DISTANCE_ENUM) AS distance,
    viajes AS n_trips,
    viajes_km AS trips_total_length_km,
    year AS year,
    month AS month,
    day AS day
FROM od_csv_raw;
