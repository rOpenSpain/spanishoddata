-- Create the relationships view from the relaciones_distrito_mitma.csv
CREATE OR REPLACE VIEW relations_districts_municipalities AS 
SELECT DISTINCT
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

-- Create the nt_csv_clean view with the necessary joins, recoding, and aggregation
CREATE OR REPLACE VIEW nt_csv_clean AS 
SELECT
    d.fecha AS fecha,
    CAST (CASE municipio_mitma WHEN 'NA' THEN NULL ELSE municipio_mitma END AS ZONES_ENUM) AS municipio,
    CAST (CASE numero_viajes WHEN 'NA' THEN NULL ELSE numero_viajes END AS N_TRIPS_ENUM) AS numero_viajes,
    SUM(d.personas) AS personas,
    CAST(d.year AS INTEGER) AS ano,
    CAST(d.month AS INTEGER) AS mes,
    CAST(d.day AS INTEGER) AS dia
FROM 
    nt_csv_raw d
LEFT JOIN 
    relations_districts_municipalities r ON d.distrito = r.distrito_mitma 
GROUP BY 
    d.fecha,
    r.municipio_mitma,
    d.numero_viajes,
    d.year,
    d.month,
    d.day;
