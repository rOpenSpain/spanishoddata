# Get latest file list from the XML for MITMA open mobility data v2 (2022 onwards)

Get latest file list from the XML for MITMA open mobility data v2 (2022
onwards)

## Usage

``` r
spod_get_latest_v2_file_list(
  data_dir = spod_get_data_dir(),
  xml_url = "https://movilidad-opendata.mitma.es/RSS.xml"
)
```

## Arguments

- data_dir:

  The directory where the data is stored. Defaults to the value returned
  by
  [`spod_get_data_dir()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_get_data_dir.md).

- xml_url:

  The URL of the XML file to download. Defaults to
  "https://movilidad-opendata.mitma.es/RSS.xml".

## Value

The path to the downloaded XML file.
