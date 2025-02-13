CREATE VIEW rcm_csv_raw AS SELECT *
    /* csv_folder needs to be replaced with a valid path
    in R use glue::glue() */
    FROM read_csv_auto('{csv_folder}**/*.csv.gz', delim='|', header=TRUE, hive_partitioning=TRUE,
    columns={{
    'mes': 'DATE',
    'origen': 'VARCHAR',
    'destino': 'VARCHAR',
    'edad': 'VARCHAR',
    'sexo': 'VARCHAR',
    'residencia': 'VARCHAR',
    'recurrencia': 'VARCHAR',
    'personas': 'DOUBLE'
    }},
    dateformat='%Y%m');
