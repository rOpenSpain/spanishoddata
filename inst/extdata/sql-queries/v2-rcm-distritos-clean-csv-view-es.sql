CREATE VIEW rcm_csv_clean AS SELECT
    CAST(year AS INTEGER) AS ano,
    CAST(month AS INTEGER) AS mes,
    CAST (origen AS ZONES_ENUM) AS origen,
    CAST (destino AS ZONES_ENUM) AS destino,
    CAST (CASE edad
        WHEN 'NA' THEN NULL
        WHEN '0-25' THEN '0-25'
        WHEN '25-45' THEN '25-45'
        WHEN '45-65' THEN '45-65'
        WHEN '65-100' THEN '65-100'
        ELSE NULL
        END AS AGE_ENUM)
        AS edad,
    CAST (CASE sexo
    -- TODO: report bug upstream
        WHEN 'NA' THEN NULL
        WHEN '' THEN NULL
        WHEN 'M' THEN 'mujer'
        WHEN 'mujer' THEN 'mujer'
        WHEN 'H' THEN 'hombre'
        WHEN 'hombre' THEN 'hombre'
        ELSE sexo
        END AS SEX_ENUM)
        AS sexo,
    CAST(residencia AS INE_PROV_CODE_ENUM) AS residencia,
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
        END AS INE_PROV_NAME_ENUM)
        AS residencia_nombre,
    CAST( recurrencia AS TRIP_RECURRENCE_ENUM) AS recurrencia,
    personas,
    FROM rcm_csv_raw
