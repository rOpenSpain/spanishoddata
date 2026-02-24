test_that("spod_get_zones validation works", {
  test_dir <- setup_test_data_dir()
  withr::defer(unlink(test_dir, recursive = TRUE))
  withr::local_envvar(c("SPANISH_OD_DATA_DIR" = test_dir))

  expect_error(
    spod_get_zones(zones = "districts", ver = 3),
    "Invalid version number"
  )
  expect_error(spod_get_zones(zones = "invalid"), "Must be element of set")
})

test_that("spod_clean_zones_v1 throws error on missing file", {
  expect_error(
    spod_clean_zones_v1("non_existent_file.shp", zones = "districts"),
    "File does not exist"
  )
})

test_that("spod_clean_zones_v2 throws error on missing file", {
  expect_error(
    spod_clean_zones_v2("distritos/non_existent_file.shp"),
    "File does not exist"
  )
})

test_that("spod_get_zones dispatcher works (mocked)", {
  local_mocked_bindings(
    spod_get_zones_v1 = function(...) {
      sf::st_sf(id = "1", geometry = sf::st_sfc(sf::st_point(c(0, 0))))
    },
    spod_get_zones_v2 = function(...) {
      sf::st_sf(id = "2", geometry = sf::st_sfc(sf::st_point(c(0, 0))))
    }
  )

  v1 <- spod_get_zones(zones = "districts", ver = 1, quiet = TRUE)
  expect_equal(v1$id, "1")

  v2 <- spod_get_zones(zones = "districts", ver = 2, quiet = TRUE)
  expect_equal(v2$id, "2")
})

test_that("spod_clean_zones_v1 handles invalid geometries", {
  # Create a simple valid sf object first
  valid_poly <- sf::st_polygon(list(matrix(
    c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
    ncol = 2,
    byrow = TRUE
  )))
  # Create an invalid one (self-intersection)
  invalid_poly <- sf::st_polygon(list(matrix(
    c(0, 0, 1, 1, 0, 1, 1, 0, 0, 0),
    ncol = 2,
    byrow = TRUE
  )))

  mock_sf <- sf::st_sf(
    ID = c("1", "2"),
    geometry = sf::st_sfc(valid_poly, invalid_poly)
  )

  # Mock sf::read_sf
  local_mocked_bindings(
    read_sf = function(...) mock_sf,
    .package = "sf"
  )

  # Mock fs::file_exists
  local_mocked_bindings(
    file_exists = function(...) TRUE,
    .package = "fs"
  )

  # Mock readr::read_delim
  local_mocked_bindings(
    read_delim = function(...) {
      # Return minimal structure expected by joins
      if (grepl("relaciones_distrito", ..1)) {
        return(data.frame(
          distrito_mitma = "1",
          municipio_mitma = "101",
          census_district = "1"
          # district_mitma removed to avoid duplication after renaming
        ))
      } else if (grepl("relaciones_municipio", ..1)) {
        return(data.frame(
          municipio_mitma = "101",
          municipality = "Muni1",
          census_district = "1"
        ))
      }
      return(data.frame())
    },
    .package = "readr"
  )

  # Also need to mock spod_get_zones_v2 as it is called inside clean_zones_v1 for metadata
  local_mocked_bindings(
    spod_get_zones_v2 = function(...) {
      sf::st_sf(
        id = "1",
        name = "Zone1",
        geometry = sf::st_sfc(valid_poly)
      )
    }
  )

  cleaned <- spod_clean_zones_v1("dummy_path.shp", "districts")

  # Check if invalid geometry was fixed (st_make_valid usually splits strict bowtie into multipolygon or fixes it)
  expect_true(all(sf::st_is_valid(cleaned)))
})

test_that("spod_clean_zones_v1 aggregates relations data correctly for districts", {
  valid_poly <- sf::st_polygon(list(matrix(
    c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
    ncol = 2,
    byrow = TRUE
  )))
  mock_sf <- sf::st_sf(
    ID = c("D001", "D002"),
    geometry = sf::st_sfc(valid_poly, valid_poly)
  )

  local_mocked_bindings(
    read_sf = function(...) mock_sf,
    .package = "sf"
  )

  local_mocked_bindings(
    file_exists = function(...) TRUE,
    .package = "fs"
  )

  # Mock relations to test aggregation
  local_mocked_bindings(
    read_delim = function(...) {
      if (grepl("relaciones_distrito", ..1)) {
        return(data.frame(
          distrito_mitma = c("D001", "D001", "D002"),
          municipio_mitma = c("M001", "M001", "M002"),
          census_district = c("C001", "C002", "C003")
        ))
      } else if (grepl("relaciones_municipio", ..1)) {
        return(data.frame(
          municipio_mitma = c("M001", "M002"),
          municipality = c("500", "501"),
          census_district = c("C001", "C003")
        ))
      }
      return(data.frame())
    },
    .package = "readr"
  )

  local_mocked_bindings(
    spod_get_zones_v2 = function(...) {
      sf::st_sf(
        id = c("D001", "D002"),
        name = c("Name1", "Name2"),
        geometry = sf::st_sfc(valid_poly, valid_poly)
      )
    }
  )

  cleaned <- spod_clean_zones_v1("dummy.shp", "distritos")

  expect_true("census_districts" %in% names(cleaned))
  expect_true("municipalities_mitma" %in% names(cleaned))
  expect_equal(nrow(cleaned), 2)
})

test_that("spod_clean_zones_v1 aggregates relations data correctly for municipalities", {
  valid_poly <- sf::st_polygon(list(matrix(
    c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
    ncol = 2,
    byrow = TRUE
  )))
  mock_sf <- sf::st_sf(
    ID = c("M001", "M002"),
    geometry = sf::st_sfc(valid_poly, valid_poly)
  )

  local_mocked_bindings(
    read_sf = function(...) mock_sf,
    .package = "sf"
  )

  local_mocked_bindings(
    file_exists = function(...) TRUE,
    .package = "fs"
  )

  local_mocked_bindings(
    read_delim = function(...) {
      if (grepl("relaciones_distrito", ..1)) {
        return(data.frame(
          distrito_mitma = c("D001", "D002"),
          municipio_mitma = c("M001", "M002"),
          census_district = c("C001", "C002")
        ))
      } else if (grepl("relaciones_municipio", ..1)) {
        return(data.frame(
          municipio_mitma = c("M001", "M002"),
          municipality = c("500", "501"),
          census_district = c("C001", "C002")
        ))
      }
      return(data.frame())
    },
    .package = "readr"
  )

  local_mocked_bindings(
    spod_get_zones_v2 = function(...) {
      sf::st_sf(
        id = c("M001", "M002"),
        name = c("Name1", "Name2"),
        geometry = sf::st_sfc(valid_poly, valid_poly)
      )
    }
  )

  cleaned <- spod_clean_zones_v1("dummy.shp", "municipios")

  expect_true("municipalities" %in% names(cleaned))
  expect_true("districts_mitma" %in% names(cleaned))
  expect_equal(nrow(cleaned), 2)
})

test_that("spod_clean_zones_v2 joins population and name data correctly", {
  valid_poly <- sf::st_polygon(list(matrix(
    c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
    ncol = 2,
    byrow = TRUE
  )))
  mock_sf <- sf::st_sf(
    ID = c("01001", "01002"),
    geometry = sf::st_sfc(valid_poly, valid_poly)
  )

  local_mocked_bindings(
    read_sf = function(...) mock_sf,
    .package = "sf"
  )

  local_mocked_bindings(
    file_exists = function(...) TRUE,
    .package = "fs"
  )

  # Mock population and name files
  local_mocked_bindings(
    read_delim = function(file, ...) {
      if (grepl("poblacion", file)) {
        return(data.frame(
          id = c("01001", "01002"),
          population = c(1000L, 2000L)
        ))
      } else if (grepl("nombres", file)) {
        return(data.frame(id = c("01001", "01002"), name = c("Zone1", "Zone2")))
      } else if (grepl("relacion_ine", file)) {
        return(data.frame(
          distrito_mitma = c("01001", "01002"),
          seccion_ine = c("S1", "S2"),
          distrito_ine = c("D1", "D2"),
          municipio_ine = c("M1", "M2"),
          municipio_mitma = c("M001", "M002"),
          gau_mitma = c("G1", "G2")
        ))
      }
      return(data.frame())
    },
    .package = "readr"
  )

  # Mock download zones v1 (called for v1-v2 mapping)
  local_mocked_bindings(
    spod_download_zones_v1 = function(...) NULL
  )

  # Mock dir_ls for v1 zones
  local_mocked_bindings(
    dir_ls = function(...) "mock_v1.shp",
    .package = "fs"
  )

  cleaned <- spod_clean_zones_v2("mock_path/zonificacion_distritos.shp")

  expect_true("population" %in% names(cleaned))
  expect_true("name" %in% names(cleaned))
  expect_equal(cleaned$population, c(1000L, 2000L))
  expect_equal(cleaned$name, c("Zone1", "Zone2"))
})

test_that("spod_clean_zones_v2 handles GAU zones (no v1 mapping)", {
  valid_poly <- sf::st_polygon(list(matrix(
    c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
    ncol = 2,
    byrow = TRUE
  )))
  mock_sf <- sf::st_sf(ID = c("G001"), geometry = sf::st_sfc(valid_poly))

  local_mocked_bindings(
    read_sf = function(...) mock_sf,
    .package = "sf"
  )

  local_mocked_bindings(
    file_exists = function(...) TRUE,
    .package = "fs"
  )

  local_mocked_bindings(
    read_delim = function(file, ...) {
      if (grepl("poblacion", file)) {
        return(data.frame(id = "G001", population = 5000L))
      } else if (grepl("nombres", file)) {
        return(data.frame(id = "G001", name = "GauZone"))
      } else if (grepl("relacion_ine", file)) {
        return(data.frame(
          gau_mitma = "G001",
          seccion_ine = "S1",
          distrito_ine = "D1",
          municipio_ine = "M1",
          municipio_mitma = "M001",
          distrito_mitma = "D001"
        ))
      }
      return(data.frame())
    },
    .package = "readr"
  )

  cleaned <- spod_clean_zones_v2("mock_path/zonificacion_gau.shp")

  expect_true("population" %in% names(cleaned))
  expect_true("name" %in% names(cleaned))
  expect_false("district_ids_in_v1" %in% names(cleaned)) # GAU should not have v1 mapping
})

test_that("spod_download_zones_v1 calls download and unzip logic", {
  test_dir <- withr::local_tempdir()

  # Mock metadata
  metadata <- tibble::tibble(
    target_url = c(
      "https://mock/relaciones_distrito_mitma.csv",
      "https://mock/relaciones_municipio_mitma.csv",
      "https://mock/zonificacion_distritos.zip"
    ),
    local_path = c(
      file.path(test_dir, "raw_data_cache/v1/relaciones_distrito_mitma.csv"),
      file.path(test_dir, "raw_data_cache/v1/relaciones_municipio_mitma.csv"),
      file.path(test_dir, "raw_data_cache/v1/zonificacion_distritos.zip")
    )
  )

  local_mocked_bindings(
    spod_available_data = function(...) metadata
  )

  # Mock file_exists to say files don't exist
  local_mocked_bindings(
    file_exists = function(path) {
      # Return FALSE for all paths to trigger download
      return(rep(FALSE, length(path)))
    },
    .package = "fs"
  )

  download_called <- FALSE
  unzip_called <- FALSE

  # Mock download
  local_mocked_bindings(
    spod_download_in_batches = function(files) {
      download_called <<- TRUE
      # Create dummy files
      for (p in files$local_path) {
        dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
        writeLines("test", p)
      }
      return(files)
    }
  )

  # Mock unzip
  local_mocked_bindings(
    unzip = function(zipfile, exdir, ...) {
      unzip_called <<- TRUE
      # Create a dummy file in the extraction directory
      dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
      writeLines("unzipped", file.path(exdir, "test.shp"))
    },
    .package = "utils"
  )

  result <- spod_download_zones_v1(
    zones = "distritos",
    data_dir = test_dir,
    quiet = TRUE,
    metadata = metadata
  )

  expect_true(download_called)
  expect_true(unzip_called)
  expect_true(length(result) > 0)
})
