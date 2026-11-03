gefs_default_bounds <- c(west = -118, east = -93, south = 23, north = 35)

# Check that all packages needed by the current pipeline stage are installed.
.require_gefs_packages <- function(packages) {
  unavailable <- packages[
    !vapply(packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
  ]
  if (length(unavailable)) {
    stop("Install required package(s): ", paste(unavailable, collapse = ", "),
         call. = FALSE)
  }
}

# Validate and normalize the geographic extent used in NOMADS requests.
.validate_gefs_bounds <- function(bounds) {
  required_names <- c("west", "east", "south", "north")
  if (!is.numeric(bounds) || !all(required_names %in% names(bounds))) {
    stop("`bounds` must contain numeric west, east, south, and north values.",
         call. = FALSE)
  }
  bounds <- bounds[required_names]
  if (any(!is.finite(bounds)) || bounds["west"] >= bounds["east"] ||
      bounds["south"] >= bounds["north"]) {
    stop("`bounds` do not describe a valid geographic extent.", call. = FALSE)
  }
  bounds
}

# Build the NOAA filename for one ensemble member and forecast hour.
.gefs_file_name <- function(member, cycle, forecast_hour) {
  prefix <- if (member == "mean") "geavg" else member
  sprintf("%s.t%02dz.pgrb2a.0p50.f%03d", prefix, cycle, forecast_hour)
}

# Build a NOMADS filter URL for one requested GEFS subset.
.gefs_nomads_url <- function(date, cycle, member, forecast_hour, bounds) {
  bounds <- .validate_gefs_bounds(bounds)
  date <- format(as.Date(date), "%Y%m%d")
  directory <- sprintf("/gefs.%s/%02d/atmos/pgrb2ap5", date, cycle)
  query <- c(
    file = .gefs_file_name(member, cycle, forecast_hour),
    lev_2_m_above_ground = "on",
    lev_10_m_above_ground = "on",
    lev_surface = "on",
    var_TMP = "on",
    var_TMAX = "on",
    var_TMIN = "on",
    var_UGRD = "on",
    var_VGRD = "on",
    var_APCP = "on",
    subregion = "",
    toplat = unname(bounds["north"]),
    leftlon = unname(bounds["west"]),
    rightlon = unname(bounds["east"]),
    bottomlat = unname(bounds["south"]),
    dir = directory
  )
  encoded_query <- paste(
    utils::URLencode(names(query), reserved = TRUE),
    utils::URLencode(as.character(query), reserved = TRUE),
    sep = "=",
    collapse = "&"
  )
  paste0(
    "https://nomads.ncep.noaa.gov/cgi-bin/filter_gefs_atmos_0p50a.pl?",
    encoded_query
  )
}

# Identify a cached file that is large enough and has a valid GRIB header.
.is_grib_file <- function(path) {
  file.exists(path) && file.info(path)$size > 100L &&
    identical(rawToChar(readBin(path, "raw", n = 4L)), "GRIB")
}

# Download one file to a temporary path, validate it, and move it into cache.
.download_gefs_file <- function(url, destination, retries = 3L) {
  temporary <- paste0(destination, ".download")
  on.exit(unlink(temporary), add = TRUE)
  for (attempt in seq_len(retries)) {
    result <- try(
      suppressWarnings(utils::download.file(
        url, temporary, mode = "wb", quiet = TRUE, method = "libcurl"
      )),
      silent = TRUE
    )
    if (!inherits(result, "try-error") && .is_grib_file(temporary)) {
      if (!file.rename(temporary, destination)) {
        stop("Could not move downloaded file to ", destination, ".", call. = FALSE)
      }
      return(destination)
    }
  }
  stop("GEFS download failed after ", retries, " attempts: ", url, call. = FALSE)
}

#' Download a GEFS forecast subset from NOAA NOMADS
#'
#' @param date Model initialization date. Defaults to the current UTC date.
#' @param cycle Model cycle hour; the operational release uses 00Z.
#' @param members GEFS member identifiers. Defaults to the control and all 30
#'   perturbed members.
#' @param forecast_hours Forecast lead hours. Defaults to six-hour intervals
#'   through day 16.
#' @param bounds Named west, east, south, and north coordinates.
#' @param output_directory Directory used to cache raw GRIB2 subsets.
#' @param retries Number of attempts for each NOAA request.
#' @return A data frame describing the downloaded files.
download_gefs_forecast <- function(
    date = as.Date(format(Sys.time(), tz = "UTC")),
    cycle = 0L,
    members = c("gec00", sprintf("gep%02d", 1:30)),
    forecast_hours = seq.int(0L, 384L, by = 6L),
    bounds = gefs_default_bounds,
    output_directory = "data/gefs",
    retries = 3L) {
  # Validate inputs and create the run-specific raw-data cache.
  .require_gefs_packages("terra")
  date <- as.Date(date)
  bounds <- .validate_gefs_bounds(bounds)
  if (is.na(date) || length(cycle) != 1L || !cycle %in% c(0L, 6L, 12L, 18L)) {
    stop("Provide a valid date and a cycle of 0, 6, 12, or 18.", call. = FALSE)
  }
  if (!all(grepl("^(gec00|gep(0[1-9]|[12][0-9]|30))$", members))) {
    stop("Unknown GEFS member identifier.", call. = FALSE)
  }
  if (any(forecast_hours < 0L | forecast_hours > 384L |
          forecast_hours %% 6L != 0L)) {
    stop("Forecast hours must be six-hour intervals from 0 through 384.",
         call. = FALSE)
  }

  run <- sprintf("%s_%02dz", format(date, "%Y%m%d"), cycle)
  raw_directory <- file.path(output_directory, run, "raw")
  dir.create(raw_directory, recursive = TRUE, showWarnings = FALSE)
  requests <- expand.grid(
    member = members,
    forecast_hour = as.integer(forecast_hours),
    stringsAsFactors = FALSE
  )
  requests$file <- file.path(
    raw_directory,
    sprintf("%s_f%03d.grib2", requests$member, requests$forecast_hour)
  )

  # Reuse valid cached files; only missing or invalid files reach NOMADS.
  for (index in seq_len(nrow(requests))) {
    destination <- requests$file[index]
    if (!.is_grib_file(destination)) {
      url <- .gefs_nomads_url(
        date, cycle, requests$member[index], requests$forecast_hour[index], bounds
      )
      .download_gefs_file(url, destination, retries)
    }
  }

  requests$date <- date
  requests$cycle <- cycle
  requests$valid_time <- as.POSIXct(date, tz = "UTC") +
    cycle * 3600 + requests$forecast_hour * 3600
  requests <- requests[c(
    "date", "cycle", "member", "forecast_hour", "valid_time", "file"
  )]
  # Record the complete request set, including files reused from cache.
  manifest_file <- file.path(output_directory, run, "download_manifest.csv")
  utils::write.csv(requests, manifest_file, row.names = FALSE)
  requests
}

# Return the first raster layer whose name matches a requested field pattern.
.gefs_layer <- function(data, pattern) {
  matches <- grep(pattern, names(data), ignore.case = TRUE)
  if (!length(matches)) NULL else data[[matches[1L]]]
}

# Read meteorological fields from one GRIB file and convert units for products.
.read_gefs_fields <- function(path) {
  data <- terra::rast(path)
  temperature <- .gefs_layer(data, "; Temperature \\[C\\]")
  maximum_temperature <- .gefs_layer(data, "Maximum temperature")
  minimum_temperature <- .gefs_layer(data, "Minimum temperature")
  u_wind <- .gefs_layer(data, "u-component of wind")
  v_wind <- .gefs_layer(data, "v-component of wind")
  precipitation <- .gefs_layer(data, "Total precipitation")
  wind_speed <- if (!is.null(u_wind) && !is.null(v_wind)) {
    sqrt(u_wind^2 + v_wind^2)
  } else {
    NULL
  }
  list(
    temperature_mean_f = if (is.null(temperature)) NULL else temperature * 9 / 5 + 32,
    temperature_max_f = if (is.null(maximum_temperature)) NULL else {
      maximum_temperature * 9 / 5 + 32
    },
    temperature_min_f = if (is.null(minimum_temperature)) NULL else {
      minimum_temperature * 9 / 5 + 32
    },
    precipitation_mm = precipitation,
    wind_max_mph = if (is.null(wind_speed)) NULL else wind_speed * 2.236936
  )
}

# Collapse a stack of rasters to one raster using the requested daily operation.
.aggregate_rasters <- function(rasters, method) {
  if (!length(rasters)) return(NULL)
  data <- terra::rast(rasters)
  switch(
    method,
    mean = terra::app(data, mean, na.rm = TRUE),
    min = terra::app(data, min, na.rm = TRUE),
    max = terra::app(data, max, na.rm = TRUE),
    sum = terra::app(data, sum, na.rm = TRUE)
  )
}

# Combine each member's six-hour fields into daily member-level products.
.daily_member_products <- function(member_manifest, timezone) {
  fields <- lapply(member_manifest$file, .read_gefs_fields)
  local_day <- as.Date(format(member_manifest$valid_time, tz = timezone))
  period_counts <- table(local_day)
  days <- as.Date(names(period_counts[period_counts >= 4L]))
  methods <- c(
    temperature_mean_f = "mean",
    temperature_max_f = "max",
    temperature_min_f = "min",
    precipitation_mm = "sum",
    wind_max_mph = "max"
  )
  products <- list()
  for (day in days) {
    forecast_day <- as.Date(day, origin = "1970-01-01")
    rows <- which(local_day == forecast_day)
    for (variable in names(methods)) {
      rasters <- Filter(Negate(is.null), lapply(fields[rows], `[[`, variable))
      product <- .aggregate_rasters(rasters, methods[[variable]])
      if (!is.null(product)) {
        products[[paste(forecast_day, variable, sep = "|")]] <- product
      }
    }
  }
  products
}

# Calculate one ensemble statistic, including threshold exceedance probabilities.
.ensemble_statistic <- function(data, statistic) {
  if (statistic == "median") {
    return(terra::app(data, stats::median, na.rm = TRUE))
  }
  if (startsWith(statistic, "probability_gt_") ||
      startsWith(statistic, "probability_lt_")) {
    direction <- if (startsWith(statistic, "probability_gt_")) ">" else "<"
    probability <- as.numeric(sub("^probability_(gt|lt)_", "", statistic))
    return(terra::app(data, function(values) {
      if (direction == ">") {
        mean(values > probability, na.rm = TRUE) * 100
      } else {
        mean(values < probability, na.rm = TRUE) * 100
      }
    }))
  }
  quantile_probability <- c(p10 = 0.1, p90 = 0.9)[[statistic]]
  terra::app(data, function(values) {
    stats::quantile(values, quantile_probability, na.rm = TRUE, names = FALSE)
  })
}

#' Process GEFS members into daily ensemble forecast products
#'
#' @param manifest A manifest returned by `download_gefs_forecast()`.
#' @param timezone IANA timezone used to define operational days.
#' @param thresholds Named thresholds for maximum temperature in degrees F,
#'   daily precipitation in mm, and maximum wind in mph.
#' @param output_directory Directory for member and ensemble GeoTIFF files.
#' @return A data frame describing the ensemble products.
process_gefs_forecast <- function(
    manifest,
    timezone = "America/Denver",
    thresholds = c(
      temperature_max_f = 20,
      precipitation_mm = 25.4,
      wind_max_mph = 25
    ),
    output_directory = NULL) {
  # Validate the manifest and processing configuration before reading rasters.
  .require_gefs_packages("terra")
  required_columns <- c("date", "cycle", "member", "valid_time", "file")
  if (!is.data.frame(manifest) || !all(required_columns %in% names(manifest))) {
    stop("`manifest` must be returned by `download_gefs_forecast()`.", call. = FALSE)
  }
  if (!all(file.exists(manifest$file))) {
    stop("One or more files in the download manifest are missing.", call. = FALSE)
  }
  if (!timezone %in% OlsonNames()) {
    stop("`timezone` must be a recognized IANA timezone.", call. = FALSE)
  }
  expected_thresholds <- c("temperature_max_f", "precipitation_mm", "wind_max_mph")
  if (!is.numeric(thresholds) || !all(expected_thresholds %in% names(thresholds))) {
    stop("`thresholds` must name temperature_max_f, precipitation_mm, and wind_max_mph.",
         call. = FALSE)
  }
  run <- sprintf(
    "%s_%02dz", format(as.Date(manifest$date[1L]), "%Y%m%d"), manifest$cycle[1L]
  )
  if (is.null(output_directory)) {
    output_directory <- file.path(dirname(dirname(manifest$file[1L])), "products")
  }
  member_directory <- file.path(output_directory, "members")
  ensemble_directory <- file.path(output_directory, "ensemble")
  dir.create(member_directory, recursive = TRUE, showWarnings = FALSE)
  dir.create(ensemble_directory, recursive = TRUE, showWarnings = FALSE)

  member_rows <- list()
  row_index <- 1L
  # Read every member's raw GRIB files, aggregate them by local day, and write
  # member-level GeoTIFFs for later ensemble calculations.
  for (member in unique(manifest$member)) {
    member_manifest <- manifest[manifest$member == member, , drop = FALSE]
    daily <- .daily_member_products(member_manifest, timezone)
    for (key in names(daily)) {
      parts <- strsplit(key, "|", fixed = TRUE)[[1L]]
      destination <- file.path(
        member_directory, sprintf("%s_%s_%s.tif", member, parts[1L], parts[2L])
      )
      terra::writeRaster(daily[[key]], destination, overwrite = TRUE,
                         gdal = c("COMPRESS=DEFLATE", "PREDICTOR=3"))
      member_rows[[row_index]] <- data.frame(
        member = member,
        forecast_day = as.Date(parts[1L]),
        variable = parts[2L],
        file = destination,
        stringsAsFactors = FALSE
      )
      row_index <- row_index + 1L
    }
  }
  member_products <- do.call(rbind, member_rows)

  groups <- unique(member_products[c("forecast_day", "variable")])
  product_rows <- list()
  row_index <- 1L
  # Stack corresponding member products and write ensemble summaries and
  # threshold-probability rasters for each day and variable.
  for (index in seq_len(nrow(groups))) {
    rows <- member_products$forecast_day == groups$forecast_day[index] &
      member_products$variable == groups$variable[index]
    ensemble <- terra::rast(member_products$file[rows])
    statistics <- c("median", "p10", "p90")
    variable <- groups$variable[index]
    if (variable %in% names(thresholds)) {
      prefix <- if (variable == "temperature_max_f") {
        "probability_lt_"
      } else {
        "probability_gt_"
      }
      statistics <- c(statistics, paste0(prefix, thresholds[[variable]]))
    }
    for (statistic in statistics) {
      product <- .ensemble_statistic(ensemble, statistic)
      safe_statistic <- gsub("\\.", "p", statistic)
      destination <- file.path(
        ensemble_directory,
        sprintf("%s_%s_%s.tif", groups$forecast_day[index], variable, safe_statistic)
      )
      terra::writeRaster(product, destination, overwrite = TRUE,
                         gdal = c("COMPRESS=DEFLATE", "PREDICTOR=3"))
      product_rows[[row_index]] <- data.frame(
        run = run,
        timezone = timezone,
        forecast_day = groups$forecast_day[index],
        variable = variable,
        statistic = statistic,
        members = sum(rows),
        file = destination,
        stringsAsFactors = FALSE
      )
      row_index <- row_index + 1L
    }
  }
  products <- do.call(rbind, product_rows)
  # The product manifest is the handoff from raster processing to visualization.
  utils::write.csv(products, file.path(output_directory, "product_manifest.csv"),
                   row.names = FALSE)
  products
}

# Choose a continuous color scale for a variable or probability layer.
.gefs_palette <- function(variable, probability = FALSE) {
  if (probability) return(c("#f7fbff", "#6baed6", "#08306b"))
  switch(
    variable,
    temperature_max_f = c("#313695", "#ffffbf", "#a50026"),
    temperature_mean_f = c("#313695", "#ffffbf", "#a50026"),
    temperature_min_f = c("#313695", "#ffffbf", "#a50026"),
    precipitation_mm = c("#ffffff", "#74c476", "#00441b"),
    wind_max_mph = c("#ffffff", "#fdae6b", "#7f0000")
  )
}

# Convert internal variable names into labels shown in maps and charts.
.gefs_variable_label <- function(variable) {
  c(
    temperature_mean_f = "Mean temperature (F)",
    temperature_max_f = "Maximum temperature (F)",
    temperature_min_f = "Minimum temperature (F)",
    precipitation_mm = "Precipitation (mm)",
    wind_max_mph = "Maximum sustained wind (mph)"
  )[[variable]]
}

#' Create an interactive daily GEFS ensemble map
#'
#' @param products A manifest returned by `process_gefs_forecast()`.
#' @param forecast_day Day to display; defaults to the first complete day.
#' @param output_file Destination HTML file.
#' @param selfcontained Whether to embed widget dependencies.
#' @return The Leaflet widget, invisibly.
create_gefs_map <- function(
    products,
    forecast_day = sort(unique(products$forecast_day))[1L],
    output_file = "output/gefs_forecast_map.html",
    selfcontained = FALSE) {
  # Select the median and threshold layers for one forecast day.
  .require_gefs_packages(c("terra", "leaflet", "htmlwidgets"))
  forecast_day <- as.Date(forecast_day)
  selected <- products[
    products$forecast_day == forecast_day &
      (products$statistic == "median" |
       startsWith(products$statistic, "probability_gt_") |
       startsWith(products$statistic, "probability_lt_")),
    , drop = FALSE
  ]
  if (!nrow(selected)) {
    stop("No GEFS products are available for ", forecast_day, ".", call. = FALSE)
  }
  map <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE))
  map <- leaflet::addProviderTiles(map, "Esri.WorldGrayCanvas", group = "Basemap")
  groups <- character(nrow(selected))
  # Convert each selected GeoTIFF to a Leaflet raster overlay with its legend.
  for (index in seq_len(nrow(selected))) {
    probability <- startsWith(selected$statistic[index], "probability_gt_") ||
      startsWith(selected$statistic[index], "probability_lt_")
    label <- .gefs_variable_label(selected$variable[index])
    groups[index] <- if (probability) {
      below <- startsWith(selected$statistic[index], "probability_lt_")
      threshold <- sub("^probability_(gt|lt)_", "", selected$statistic[index])
      paste0("Probability: ", label, if (below) " < " else " > ", threshold)
    } else {
      paste0("Median: ", label)
    }
    raster <- terra::rast(selected$file[index])
    values <- terra::values(raster, na.rm = TRUE)
    domain <- if (probability) c(0, 100) else range(values, finite = TRUE)
    palette <- leaflet::colorNumeric(
      .gefs_palette(selected$variable[index], probability), domain = domain,
      na.color = "transparent"
    )
    map <- leaflet::addRasterImage(
      map, raster, colors = palette, opacity = 0.7, group = groups[index],
      project = TRUE, maxBytes = 8 * 1024 * 1024
    )
    map <- leaflet::addLegend(
      map, pal = palette, values = values, title = groups[index],
      group = groups[index], position = "bottomright", opacity = 0.85
    )
  }
  map <- leaflet::addLayersControl(
    map, baseGroups = "Basemap", overlayGroups = groups,
    options = leaflet::layersControlOptions(collapsed = FALSE)
  )
  if (length(groups) > 1L) map <- leaflet::hideGroup(map, groups[-1L])
  map <- leaflet::addControl(
    map,
    html = paste0(
      "<strong>NOAA GEFS daily forecast</strong><br>", forecast_day,
      "<br>Median and threshold probabilities from ", selected$members[1L],
      " members"
    ),
    position = "topleft"
  )
  # Save the widget; the forecast rasters are embedded while the basemap tiles
  # are requested by the browser from the configured provider.
  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  htmlwidgets::saveWidget(map, output_file, selfcontained = selfcontained)
  invisible(map)
}

gefs_border_locations <- data.frame(
  location = c("Nogales, AZ", "Douglas, AZ", "El Paso, TX", "Santa Teresa, NM",
               "Brownsville, TX"),
  longitude = c(-110.9342, -109.5453, -106.4850, -106.6394, -97.4975),
  latitude = c(31.3404, 31.3445, 31.7619, 31.8687, 25.9017)
)

#' Create GEFS point forecast charts for southern-border locations
#'
#' @param products A manifest returned by `process_gefs_forecast()`.
#' @param locations Data frame with location, longitude, and latitude columns.
#' @param output_file Destination PNG file.
#' @return The extracted chart data, invisibly.
create_gefs_point_charts <- function(
    products,
    locations = gefs_border_locations,
    output_file = "output/gefs_point_forecasts.png") {
  # Extract median and percentile values at the configured border locations.
  .require_gefs_packages(c("terra", "ggplot2"))
  required_columns <- c("location", "longitude", "latitude")
  if (!is.data.frame(locations) || !all(required_columns %in% names(locations))) {
    stop("`locations` must contain location, longitude, and latitude columns.",
         call. = FALSE)
  }
  selected <- products[
    products$statistic %in% c("median", "p10", "p90") &
      products$variable %in% c(
        "temperature_max_f", "precipitation_mm", "wind_max_mph"
      ),
    , drop = FALSE
  ]
  points <- as.matrix(locations[c("longitude", "latitude")])
  extracted <- lapply(seq_len(nrow(selected)), function(index) {
    extracted_values <- terra::extract(terra::rast(selected$file[index]), points)
    values <- extracted_values[[ncol(extracted_values)]]
    data.frame(
      location = locations$location,
      forecast_day = as.Date(selected$forecast_day[index]),
      variable = selected$variable[index],
      statistic = selected$statistic[index],
      value = values,
      stringsAsFactors = FALSE
    )
  })
  chart_data <- do.call(rbind, extracted)
  chart_data <- reshape(
    chart_data,
    idvar = c("location", "forecast_day", "variable"),
    timevar = "statistic", direction = "wide"
  )
  names(chart_data) <- sub("^value\\.", "", names(chart_data))
  chart_data$variable <- factor(
    chart_data$variable,
    levels = c("temperature_max_f", "precipitation_mm", "wind_max_mph"),
    labels = c("Maximum temperature (F)", "Daily precipitation (mm)",
               "Maximum sustained wind (mph)")
  )
  # Plot one panel per variable and location, with the ensemble range shaded.
  forecast_day <- location <- median <- p10 <- p90 <- NULL
  plot <- ggplot2::ggplot(
    chart_data,
    ggplot2::aes(x = forecast_day, y = median, group = location, color = location)
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = p10, ymax = p90, fill = location),
      alpha = 0.12, color = NA
    ) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::geom_point(size = 1.2) +
    ggplot2::facet_grid(variable ~ location, scales = "free_y") +
    ggplot2::scale_x_date(date_breaks = "4 days", date_labels = "%b %d") +
    ggplot2::labs(x = NULL, y = NULL, color = NULL, fill = NULL,
                  title = "NOAA GEFS southern-border point forecasts",
                  subtitle = "Median with 10th-90th percentile ensemble range") +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(output_file, plot, width = 14, height = 8, dpi = 160)
  invisible(chart_data)
}

#' Download, process, and visualize the operational GEFS release
#'
#' @param date Model initialization date.
#' @param cycle Model cycle hour.
#' @param bounds Southern-border download bounds.
#' @param timezone Timezone used for daily aggregation.
#' @param thresholds Operational probability thresholds.
#' @param data_directory Raw and processed data directory.
#' @param output_directory Visualization output directory.
#' @param members Ensemble members to download.
#' @param forecast_hours Forecast lead hours to download.
#' @return A list containing download and product manifests.
create_gefs_release <- function(
    date = as.Date(format(Sys.time(), tz = "UTC")),
    cycle = 0L,
    bounds = gefs_default_bounds,
    timezone = "America/Denver",
    thresholds = c(
      temperature_max_f = 20,
      precipitation_mm = 25.4,
      wind_max_mph = 25
    ),
    data_directory = "data/gefs",
    output_directory = "output/gefs",
    members = c("gec00", sprintf("gep%02d", 1:30)),
    forecast_hours = seq.int(0L, 384L, by = 6L)) {
  # Stage 1: download or reuse the raw GEFS files for this model run.
  downloads <- download_gefs_forecast(
    date = date, cycle = cycle, members = members,
    forecast_hours = forecast_hours, bounds = bounds,
    output_directory = data_directory
  )
  run <- sprintf("%s_%02dz", format(as.Date(date), "%Y%m%d"), cycle)
  # Stage 2: turn raw fields into daily member and ensemble GeoTIFF products.
  products <- process_gefs_forecast(
    downloads, timezone = timezone, thresholds = thresholds,
    output_directory = file.path(data_directory, run, "products")
  )
  product_days <- sort(unique(products$forecast_day))
  map_directory <- file.path(output_directory, "daily_maps")
  # Stage 3: write one interactive map for every available forecast day.
  for (forecast_day in product_days) {
    map_day <- as.Date(forecast_day, origin = "1970-01-01")
    create_gefs_map(
      products, forecast_day = map_day,
      output_file = file.path(map_directory, paste0(map_day, ".html"))
    )
  }
  first_map_day <- as.Date(product_days[1L], origin = "1970-01-01")
  # Also write a convenient first-day map at the top-level output path.
  create_gefs_map(
    products, forecast_day = first_map_day,
    output_file = file.path(output_directory, "forecast_map.html")
  )
  # Stage 4: write the multi-location point forecast chart.
  create_gefs_point_charts(
    products, output_file = file.path(output_directory, "point_forecasts.png")
  )
  invisible(list(downloads = downloads, products = products))
}