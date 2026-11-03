forecast_layer_names <- c(
  temperature_6_10_day = "Temperature: 6–10 day",
  temperature_8_14_day = "Temperature: 8–14 day",
  temperature_3_4_week = "Temperature: 3–4 week",
  temperature_30_90_day = "Temperature: 30–90 day",
  precipitation_6_10_day = "Precipitation: 6–10 day",
  precipitation_8_14_day = "Precipitation: 8–14 day",
  precipitation_3_4_week = "Precipitation: 3–4 week",
  precipitation_30_90_day = "Precipitation: 30–90 day"
)

.validate_forecast_files <- function(forecast_files) {
  if (!is.character(forecast_files) || is.null(names(forecast_files))) {
    stop("`forecast_files` must be a named character vector.", call. = FALSE)
  }

  required_names <- setdiff(
    names(forecast_layer_names),
    c(
      "temperature_3_4_week", "precipitation_3_4_week",
      "temperature_30_90_day", "precipitation_30_90_day"
    )
  )
  missing_names <- setdiff(required_names, names(forecast_files))
  extra_names <- setdiff(names(forecast_files), names(forecast_layer_names))
  if (length(missing_names) || length(extra_names)) {
    details <- c(
      if (length(missing_names)) paste("missing:", paste(missing_names, collapse = ", ")),
      if (length(extra_names)) paste("unknown:", paste(extra_names, collapse = ", "))
    )
    stop(paste("Invalid forecast layer names (", paste(details, collapse = "; "), ").", sep = ""),
         call. = FALSE)
  }

  forecast_files <- forecast_files[intersect(names(forecast_layer_names), names(forecast_files))]
  missing_files <- forecast_files[!file.exists(forecast_files)]
  if (length(missing_files)) {
    stop("Forecast file(s) not found: ", paste(missing_files, collapse = ", "), call. = FALSE)
  }
  supported_extensions <- c("kmz", "zip")
  unsupported_files <- forecast_files[
    !tolower(tools::file_ext(forecast_files)) %in% supported_extensions
  ]
  if (length(unsupported_files)) {
    stop("Every forecast input must be a KMZ or ZIP file: ",
         paste(unsupported_files, collapse = ", "), call. = FALSE)
  }

  normalized_files <- normalizePath(forecast_files, mustWork = TRUE)
  names(normalized_files) <- names(forecast_files)
  normalized_files
}

.read_spatial_layers <- function(path) {
  source_files <- path
  extraction_directory <- NULL

  if (tolower(tools::file_ext(path)) %in% c("kmz", "zip")) {
    extraction_directory <- tempfile("spatial-archive-")
    dir.create(extraction_directory)
    on.exit(unlink(extraction_directory, recursive = TRUE), add = TRUE)
    archive <- utils::unzip(path, list = TRUE)
    spatial_entries <- archive$Name[
      grepl("\\.(kml|shp)$", archive$Name, ignore.case = TRUE)
    ]
    unsafe_entries <- grepl("(^|[/\\\\])\\.\\.([/\\\\]|$)|^[/\\\\]|^[A-Za-z]:",
                            archive$Name)
    if (any(unsafe_entries)) {
      stop("Unsafe path found in ", path, ".", call. = FALSE)
    }
    if (!length(spatial_entries)) {
      stop("No KML document or shapefile was found in ", path, ".", call. = FALSE)
    }
    utils::unzip(path, exdir = extraction_directory)
    source_files <- file.path(extraction_directory, spatial_entries)
  }

  spatial_layers <- unlist(lapply(source_files, function(source_file) {
    layer_names <- sf::st_layers(source_file)$name
    lapply(layer_names, function(layer_name) {
      layer <- sf::st_read(source_file, layer = layer_name, quiet = TRUE)
      if (is.na(sf::st_crs(layer))) {
        sf::st_crs(layer) <- 4326
      }
      sf::st_make_valid(sf::st_transform(layer, 4326))
    })
  }), recursive = FALSE)

  Filter(function(layer) nrow(layer) > 0, spatial_layers)
}

.feature_text <- function(data) {
  attributes <- sf::st_drop_geometry(data)
  if (!ncol(attributes)) {
    return(rep("", nrow(data)))
  }
  apply(attributes, 1, function(values) paste(values[!is.na(values)], collapse = " "))
}

.forecast_colors <- function(data, group, default_color) {
  text <- tolower(.feature_text(data))
  probability <- suppressWarnings(as.integer(sub(
    ".*?([0-9]{2,3})\\s*%.*", "\\1", text, perl = TRUE
  )))
  probability[is.na(probability) | probability < 33 | probability > 100] <- 50
  shade <- pmax(1L, pmin(68L, probability - 32L))

  if (startsWith(group, "Temperature")) {
    above <- grDevices::colorRampPalette(c("#fddbc7", "#b2182b"))(68)
    below <- grDevices::colorRampPalette(c("#d1e5f0", "#2166ac"))(68)
  } else {
    above <- grDevices::colorRampPalette(c("#d9f0d3", "#1b7837"))(68)
    below <- grDevices::colorRampPalette(c("#fee0b6", "#b35806"))(68)
  }

  colors <- rep(default_color, nrow(data))
  colors[grepl("above", text)] <- above[shade[grepl("above", text)]]
  colors[grepl("below", text)] <- below[shade[grepl("below", text)]]
  colors[grepl("near|normal", text) & !grepl("above|below", text)] <- "#bdbdbd"
  colors
}

.feature_labels <- function(data, group) {
  attributes <- sf::st_drop_geometry(data)
  preferred <- grep("^(name|label|title|description)$", names(attributes),
                    ignore.case = TRUE, value = TRUE)
  labels <- if (length(preferred)) {
    as.character(attributes[[preferred[1]]])
  } else {
    rep(group, nrow(data))
  }
  labels[is.na(labels) | labels == ""] <- group

  details <- grep("^(prob|cat)$", names(attributes),
                  ignore.case = TRUE, value = TRUE)
  if (length(details)) {
    detail_text <- vapply(seq_len(nrow(attributes)), function(index) {
      values <- vapply(details, function(column) {
        value <- attributes[[column]][index]
        if (is.na(value) || value == "") "" else paste(column, value, sep = ": ")
      }, character(1))
      paste(values[values != ""], collapse = " | ")
    }, character(1))
    labels <- ifelse(detail_text == "", labels,
                     paste(labels, detail_text, sep = " | "))
  }

  gsub("<[^>]+>", "", labels)
}

.add_spatial_layer <- function(map, data, group, color, dispersal = FALSE) {
  geometry_types <- as.character(sf::st_geometry_type(data))
  polygon_rows <- grepl("POLYGON", geometry_types)
  line_rows <- grepl("LINESTRING", geometry_types)
  point_rows <- grepl("POINT", geometry_types)
  feature_colors <- if (dispersal) rep(color, nrow(data)) else {
    .forecast_colors(data, group, color)
  }
  feature_labels <- .feature_labels(data, group)

  if (any(polygon_rows)) {
    map <- leaflet::addPolygons(
      map,
      data = data[polygon_rows, , drop = FALSE],
      group = group,
      color = feature_colors[polygon_rows],
      weight = if (dispersal) 3 else 1,
      dashArray = if (dispersal) "6" else NULL,
      fillColor = feature_colors[polygon_rows],
      fillOpacity = if (dispersal) 0.15 else 0.45,
      label = feature_labels[polygon_rows]
    )
  }
  if (any(line_rows)) {
    map <- leaflet::addPolylines(
      map,
      data = data[line_rows, , drop = FALSE],
      group = group,
      color = feature_colors[line_rows],
      weight = if (dispersal) 3 else 2,
      dashArray = if (dispersal) "6" else NULL,
      label = feature_labels[line_rows]
    )
  }
  if (any(point_rows)) {
    map <- leaflet::addCircleMarkers(
      map,
      data = data[point_rows, , drop = FALSE],
      group = group,
      color = feature_colors[point_rows],
      fillColor = feature_colors[point_rows],
      radius = 4,
      stroke = TRUE,
      fillOpacity = 0.8,
      label = feature_labels[point_rows]
    )
  }

  map
}

#' Create an interactive CPC forecast map
#'
#' @param kmz_files A named character vector containing the four required CPC
#'   forecast paths and optional 3-4 week and 30-90 day forecast paths.
#' @param dispersal_polygon Optional path to a vector file (including KMZ/KML,
#'   GeoJSON, GeoPackage, or shapefile) or an `sf` object.
#' @param output_file Destination HTML file.
#' @param selfcontained Whether to embed widget dependencies in the HTML file.
#' @return The leaflet widget, invisibly.
create_forecast_map <- function(
    kmz_files,
    dispersal_polygon = NULL,
    output_file = "output/forecast_map.html",
    selfcontained = TRUE) {
  kmz_files <- .validate_forecast_files(kmz_files)

  required_packages <- c("sf", "leaflet", "htmlwidgets")
  unavailable <- required_packages[
    !vapply(required_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
  ]
  if (length(unavailable)) {
    stop("Install required package(s): ", paste(unavailable, collapse = ", "), call. = FALSE)
  }

  forecast_layers <- lapply(kmz_files, .read_spatial_layers)
  empty_forecasts <- names(forecast_layers)[!lengths(forecast_layers)]
  if (length(empty_forecasts)) {
    stop("No spatial features found in: ", paste(empty_forecasts, collapse = ", "),
         call. = FALSE)
  }

  map <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE))
  map <- leaflet::addProviderTiles(map, "OpenStreetMap", group = "Basemap")

  colors <- ifelse(
    startsWith(names(forecast_layers), "temperature_"), "#d73027", "#4575b4"
  )
  for (index in seq_along(forecast_layers)) {
    group <- unname(forecast_layer_names[names(forecast_layers)[index]])
    for (layer in forecast_layers[[index]]) {
      map <- .add_spatial_layer(map, layer, group, colors[index])
    }
  }

  overlay_groups <- unname(forecast_layer_names[names(forecast_layers)])
  if (!is.null(dispersal_polygon)) {
    polygon_layers <- if (inherits(dispersal_polygon, "sf")) {
      if (is.na(sf::st_crs(dispersal_polygon))) {
        sf::st_crs(dispersal_polygon) <- 4326
      }
      list(sf::st_transform(dispersal_polygon, 4326))
    } else {
      if (!is.character(dispersal_polygon) || length(dispersal_polygon) != 1L ||
          !file.exists(dispersal_polygon)) {
        stop("`dispersal_polygon` must be an sf object or an existing vector file.",
             call. = FALSE)
      }
      .read_spatial_layers(normalizePath(dispersal_polygon, mustWork = TRUE))
    }
    if (!length(polygon_layers)) {
      stop("No spatial features found in the dispersal polygon.", call. = FALSE)
    }
    for (layer in polygon_layers) {
      map <- .add_spatial_layer(map, layer, "Dispersal polygon", "#7b3294", TRUE)
    }
    overlay_groups <- c(overlay_groups, "Dispersal polygon")
  }

  map <- leaflet::addLayersControl(
    map,
    baseGroups = "Basemap",
    overlayGroups = overlay_groups,
    options = leaflet::layersControlOptions(collapsed = FALSE)
  )
  initially_hidden <- setdiff(
    names(forecast_layers), c("temperature_6_10_day", "precipitation_6_10_day")
  )
  if (length(initially_hidden)) {
    map <- leaflet::hideGroup(map, unname(forecast_layer_names[initially_hidden]))
  }

  output_directory <- dirname(output_file)
  if (!dir.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE)
  }
  htmlwidgets::saveWidget(map, output_file, selfcontained = selfcontained)

  invisible(map)
}
