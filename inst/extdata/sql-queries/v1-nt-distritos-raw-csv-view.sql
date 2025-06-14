CREATE OR REPLACE VIEW nt_csv_raw AS SELECT *
    /* csv_folder needs to be replaced with a valid path
    in R use glue::glue() */
    FROM read_csv_auto('{csv_folder}**/*.csv.gz', delim='|', header=TRUE, hive_partitioning=TRUE,
    columns={{
    'fecha': 'DATE',
    'distrito': 'VARCHAR',
    'numero_viajes': 'VARCHAR',
    'personas': 'DOUBLE'
    }},
    dateformat='%Y%m%d');
