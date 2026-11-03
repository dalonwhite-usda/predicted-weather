# Load the helper functions and the create_gefs_release() pipeline.
source("R/gefs_forecast.R")

# Use an explicit environment variable when supplied; otherwise use today's
# UTC model date because GEFS run dates are defined in UTC.
date_value <- Sys.getenv(
  "GEFS_DATE", format(as.Date(format(Sys.time(), tz = "EST")), "%Y-%m-%d")
)
cycle_value <- as.integer(Sys.getenv("GEFS_CYCLE", "0"))


# # This override selects a specific cached or published model run for testing.
# date_value = as.Date("2026-11-02")

# Pass the selected run to the main pipeline, which downloads, processes, and
# visualizes the forecast.
cat("Creating NOAA GEFS release for", date_value, sprintf("%02dZ\n", cycle_value))
create_gefs_release(
  date = as.Date(date_value),
  cycle = cycle_value
)
cat("GEFS release written to output/gefs\n")
