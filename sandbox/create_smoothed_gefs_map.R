source("R/gefs_forecast.R")

run <- "20261102_00z"
forecast_day <- as.Date("2026-11-12")
manifest_file <- file.path("data", "gefs", run, "products", "product_manifest.csv")
output_file <- file.path("sandbox", "output", paste0(forecast_day, "-smoothed.html"))

products <- utils::read.csv(manifest_file, stringsAsFactors = FALSE)
products$forecast_day <- as.Date(products$forecast_day)

day_products <- products$forecast_day == forecast_day
if (!any(day_products)) {
  stop("No cached GEFS products are available for ", forecast_day, ".", call. = FALSE)
}
if (!all(file.exists(products$file[day_products]))) {
  stop("One or more cached GEFS rasters are missing for ", forecast_day, ".",
       call. = FALSE)
}

unlink(output_file)

create_gefs_map(
  products,
  forecast_day = forecast_day,
  output_file = output_file,
  selfcontained = TRUE,
  smooth_factor = 4L
)

cat("Smoothed GEFS map written to", output_file, "\n")
