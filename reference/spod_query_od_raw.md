# Internal function to query the GraphQL API for origin-destination data

Internal function to query the GraphQL API for origin-destination data

## Usage

``` r
spod_query_od_raw(
  date_fmt,
  graphql_distances,
  id_origin,
  id_destination,
  min_trips,
  graphql_query
)
```

## Arguments

- id_origin:

  A character vector specifying the origin municipalities to retrieve.
  If not provided, all origin municipalities will be included. Valid
  municipality IDs can be found in the dataset returned by
  `spod_get_zones(zones = "muni", ver = 2)`.

- id_destination:

  A character vector specifying the target municipalities to retrieve.
  If not provided, all target municipalities will be included. Valid
  municipality IDs can be found in the dataset returned by
  `spod_get_zones(zones = "muni", ver = 2)`.

- min_trips:

  A numeric value specifying the minimum number of journeys per
  origin-destination pair to retrieve. Defaults to 100 to reduce the
  amount of data returned. Can be set to 0 to retrieve all data.

## Value

A `tibble` containing the flows for the specified date, minimum number
of journeys, distances and origin-destination pairs if specified.
