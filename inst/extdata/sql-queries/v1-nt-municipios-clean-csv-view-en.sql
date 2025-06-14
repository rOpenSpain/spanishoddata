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

-- Create the nt_csv_clean view with the necessary joins, recoding, and aggregation
CREATE OR REPLACE VIEW nt_csv_clean AS 
SELECT
    d.fecha AS date,
    CAST(CASE r.municipio_mitma
        WHEN 'externo' THEN 'external'
        ELSE r.municipio_mitma
        END AS ZONES_ENUM) AS id,
    CAST(d.numero_viajes AS N_TRIPS_ENUM) AS n_trips,
    SUM(d.personas) AS n_persons,
    CAST(d.year AS INTEGER) AS year,
    CAST(d.month AS INTEGER) AS month,
    CAST(d.day AS INTEGER) AS day
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
