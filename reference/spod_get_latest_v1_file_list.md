# Get latest file list from the XML for MITMA open mobility data v1 (2020-2021)

Get latest file list from the XML for MITMA open mobility data v1
(2020-2021)

## Usage

``` r
spod_get_latest_v1_file_list(
  data_dir = spod_get_data_dir(),
  xml_url = "https://opendata-movilidad.mitma.es/RSS.xml"
)
```

## Arguments

- data_dir:

  The directory where the data is stored. Defaults to the value returned
  by
  [`spod_get_data_dir()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_get_data_dir.md).

- xml_url:

  The URL of the XML file to download. Defaults to
  "https://opendata-movilidad.mitma.es/RSS.xml".

## Value

The path to the downloaded XML file.
