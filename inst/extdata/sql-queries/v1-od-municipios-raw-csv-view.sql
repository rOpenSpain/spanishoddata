CREATE OR REPLACE VIEW od_csv_raw AS SELECT *
    /* csv_folder needs to be replaced with a valid path
    in R use glue::glue() 
    # this file actually connects to raw data with districts, because of the bugs described in
    # http://www.ekotov.pro/mitma-data-issues/issues/011-v1-tpp-mismatch-zone-ids-in-table-and-spatial-data.html
    # http://www.ekotov.pro/mitma-data-issues/issues/012-v1-tpp-district-files-in-municipality-folders.html
    # the decision was to use distrcit data and aggregate it to replicate municipal data */
    FROM read_csv_auto('{csv_folder}**/*.csv.gz', delim='|', header=TRUE, hive_partitioning=TRUE,
    columns={{
        'fecha': 'DATE',
        'origen': 'VARCHAR',
        'destino': 'VARCHAR',
        'actividad_origen': 'VARCHAR',
        'actividad_destino': 'VARCHAR',
        'residencia': 'VARCHAR',
        'edad': 'VARCHAR',
        'periodo': 'INTEGER',
        'distancia': 'VARCHAR',
        'viajes': 'DOUBLE',
        'viajes_km': 'DOUBLE'
    }},
    dateformat='%Y%m%d');
