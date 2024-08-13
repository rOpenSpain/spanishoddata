CREATE VIEW od_csv_raw AS SELECT *
    /* csv_folder needs to be replaced with a valid path
    in R use glue::glue() */
    FROM read_csv_auto('{csv_folder}**/*.csv.gz',
        delim='|',
        header=TRUE,
        hive_partitioning=TRUE,
        columns={{
            'fecha': 'DATE',
            'periodo': 'INTEGER',
            'origen': 'VARCHAR',
            'destino': 'VARCHAR',
            'distancia': 'VARCHAR',
            'actividad_origen': 'VARCHAR',
            'actividad_destino': 'VARCHAR',
            'estudio_origen_posible': 'VARCHAR',
            'estudio_destino_posible': 'VARCHAR',
            'residencia': 'VARCHAR',
            'renta': 'VARCHAR',
            'edad': 'VARCHAR',
            'sexo': 'VARCHAR',
            'viajes': 'DOUBLE',
            'viajes_km': 'DOUBLE'
    }},
    dateformat='%Y%m%d');
