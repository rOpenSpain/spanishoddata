-- Create the relationships view from the relaciones_distrito_mitma.csv
CREATE OR REPLACE VIEW relations_districts_municipalities AS 
SELECT
    distrito_mitma, 
    municipio_mitma 
FROM 
    read_csv_auto('{relations_districts_municipalities}',
    delim = '|',
    columns={{
        'distrito': 'VARCHAR',
        'distrito_mitma': 'VARCHAR',
        'municipio_mitma': 'VARCHAR'
    }}
);

-- Create the od_csv_clean view with necessary joins, recoding, and aggregation
CREATE OR REPLACE VIEW od_csv_clean AS 
SELECT
    d.fecha AS fecha,
    CAST(m1.municipio_mitma AS ZONES_ENUM) AS origen,
    CAST(m2.municipio_mitma AS ZONES_ENUM) AS destino,
    CAST(CASE d.actividad_origen
        WHEN 'casa' THEN 'home'
        WHEN 'otros' THEN 'other'
        WHEN 'trabajo_estudio' THEN 'work_or_study'
        END AS ACTIV_ENUM) AS actividad_origen,
    CAST(CASE d.actividad_destino
        WHEN 'casa' THEN 'home'
        WHEN 'otros' THEN 'other'
        WHEN 'trabajo_estudio' THEN 'work_or_study'
        END AS ACTIV_ENUM) AS actividad_destino,
    CAST(d.residencia AS INE_PROV_CODE_ENUM) AS residencia,
    CAST(CASE d.residencia
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
        END AS INE_PROV_NAME_ENUM) AS residencia_nombre,
    d.periodo AS periodo,
    CAST(d.distancia AS DISTANCE_ENUM) AS distancia,
    SUM(d.viajes) AS viajes,
    SUM(d.viajes_km) AS viajes_km,
    CAST(d.year AS INTEGER) AS ano,
    CAST(d.month AS INTEGER) AS mes,
    CAST(d.day AS INTEGER) AS dia
FROM 
    od_csv_raw d
LEFT JOIN 
    relations_districts_municipalities m1 ON d.origen = m1.distrito_mitma
LEFT JOIN 
    relations_districts_municipalities m2 ON d.destino = m2.distrito_mitma
GROUP BY 
    d.fecha, 
    m1.municipio_mitma,
    m2.municipio_mitma,
    d.actividad_origen,
    d.actividad_destino,
    d.residencia,
    d.periodo,
    d.distancia,
    d.year,
    d.month,
    d.day;
