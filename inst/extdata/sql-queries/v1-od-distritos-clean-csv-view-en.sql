CREATE VIEW od_csv_clean AS SELECT
    fecha AS date,
    CAST (CASE origen
        WHEN 'externo' THEN 'external'
        ELSE origen
        END AS ZONES_ENUM)
        AS id_origin,
    CAST (CASE destino
        WHEN 'externo' THEN 'external'
        ELSE destino
        END AS ZONES_ENUM)
        AS id_destination,
    CAST(CASE actividad_origen
        WHEN 'casa' THEN 'home'
        WHEN 'otros' THEN 'other'
        WHEN 'trabajo_estudio' THEN 'work_or_study'
        END AS ACTIV_ENUM) AS activity_origin,
    CAST(CASE actividad_destino
        WHEN 'casa' THEN 'home'
        WHEN 'otros' THEN 'other'
        WHEN 'trabajo_estudio' THEN 'work_or_study'
        END AS ACTIV_ENUM) AS activity_destination,
    CAST(residencia AS INE_PROV_CODE_ENUM) AS residence_province_ine_code,
    CAST (CASE residencia
        WHEN '01' THEN 'Araba/Álava'
        WHEN '02' THEN 'Albacete'
        WHEN '03' THEN 'Alicante/Alacant'
        WHEN '04' THEN 'Almería'
        WHEN '05' THEN 'Ávila'
        WHEN '06' THEN 'Badajoz'
        WHEN '07' THEN 'Balears, Illes'
        WHEN '08' THEN 'Barcelona'
        WHEN '09' THEN 'Burgos'
        WHEN '10' THEN 'Cáceres'
        WHEN '11' THEN 'Cádiz'
        WHEN '12' THEN 'Castellón/Castelló'
        WHEN '13' THEN 'Ciudad Real'
        WHEN '14' THEN 'Córdoba'
        WHEN '15' THEN 'Coruña, A'
        WHEN '16' THEN 'Cuenca'
        WHEN '17' THEN 'Girona'
        WHEN '18' THEN 'Granada'
        WHEN '19' THEN 'Guadalajara'
        WHEN '20' THEN 'Gipuzkoa'
        WHEN '21' THEN 'Huelva'
        WHEN '22' THEN 'Huesca'
        WHEN '23' THEN 'Jaén'
        WHEN '24' THEN 'León'
        WHEN '25' THEN 'Lleida'
        WHEN '26' THEN 'Rioja, La'
        WHEN '27' THEN 'Lugo'
        WHEN '28' THEN 'Madrid'
        WHEN '29' THEN 'Málaga'
        WHEN '30' THEN 'Murcia'
        WHEN '31' THEN 'Navarra'
        WHEN '32' THEN 'Ourense'
        WHEN '33' THEN 'Asturias'
        WHEN '34' THEN 'Palencia'
        WHEN '35' THEN 'Palmas, Las'
        WHEN '36' THEN 'Pontevedra'
        WHEN '37' THEN 'Salamanca'
        WHEN '38' THEN 'Santa Cruz de Tenerife'
        WHEN '39' THEN 'Cantabria'
        WHEN '40' THEN 'Segovia'
        WHEN '41' THEN 'Sevilla'
        WHEN '42' THEN 'Soria'
        WHEN '43' THEN 'Tarragona'
        WHEN '44' THEN 'Teruel'
        WHEN '45' THEN 'Toledo'
        WHEN '46' THEN 'Valencia/València'
        WHEN '47' THEN 'Valladolid'
        WHEN '48' THEN 'Bizkaia'
        WHEN '49' THEN 'Zamora'
        WHEN '50' THEN 'Zaragoza'
        WHEN '51' THEN 'Ceuta'
        WHEN '52' THEN 'Melilla'
        END AS INE_PROV_NAME_ENUM) AS residence_province_name,
    periodo AS hour,
    CAST(distancia AS DISTANCE_ENUM) AS distance,
    viajes AS n_trips,
    viajes_km AS trips_total_length_km,
    CAST(year AS INTEGER) AS year,
    CAST(month AS INTEGER) AS month,
    CAST(day AS INTEGER) AS day
FROM od_csv_raw;
