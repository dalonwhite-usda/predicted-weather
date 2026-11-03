# predicted-weather

Build an interactive Leaflet map of forward temperature and precipitation
forecasts for operational decision making.

## Inputs

Download the temperature and precipitation KMZ or ZIP forecasts from the
[Climate Prediction Center GIS data page][cpc] for each of these periods:

- 6–10 day
- 8–14 day
- 3–4 week
- 30–90 day

Keep the files locally (the `data/` directory is ignored by Git). The mapping
function accepts an optional dispersal polygon as an `sf` object or as any
vector file readable by `sf`, including KMZ/KML, GeoJSON, GeoPackage, and
shapefile.

## Setup

Install R and the required packages:

```r
install.packages(c("sf", "leaflet", "htmlwidgets"))
```

For a single-file HTML output, `htmlwidgets` also requires Pandoc. Set
`selfcontained = FALSE` if Pandoc is unavailable; this creates the HTML file
and a neighboring dependency directory.

## Create the map

From the repository root, run the following code in the R console. You can also
save it as a separate `.R` file and run that file. The `forecast_files` object is
created by this code and then passed to `create_forecast_map()`; the mapping
function does not read the README or automatically scan `data/active`.

```r
source("R/create_forecast_map.R")

forecast_files <- c(
  temperature_6_10_day = "data/active/610temp_20261101.kmz",
  temperature_8_14_day = "data/active/814temp_20261101.kmz",
  precipitation_6_10_day = "data/active/610prcp_20261101.kmz",
  precipitation_8_14_day = "data/active/814prcp_20261101.kmz"
)

create_forecast_map(
  forecast_files,
  output_file = "output/forecast_map.html"
)
```

Open `output/forecast_map.html` in a browser. The layer control can independently
show or hide every forecast and the dispersal polygon. The two 6–10 day layers
are shown initially; the other forecasts remain available in the control.
Forecast features are shaded by above/below-normal category and probability,
and hovering shows the feature label from the CPC file. If you have a dispersal
polygon, add `dispersal_polygon = "path/to/polygon.geojson"` to the function call.

The names on the left side of `forecast_files` are the layer keys; the
paths on the right side are the downloaded filenames. They do not need to match.
Each forecast input can be a KMZ or a ZIP containing KML or shapefile files.
The 3–4 week and 30–90 day temperature and precipitation inputs are optional.
Add any of them later using the keys `temperature_3_4_week`,
`precipitation_3_4_week`, `temperature_30_90_day`, and
`precipitation_30_90_day`. The four paths shown use the KMZ files currently in
`data/active`.

[cpc]: https://www.cpc.ncep.noaa.gov/products/GIS/GIS_DATA/

## GEFS daily forecasts

The GEFS workflow complements the CPC anomaly outlook with actual forecast
temperature, precipitation, and sustained wind. It downloads all 31 ensemble
members from the NOAA NOMADS 0.5 degree product at six-hour intervals through
forecast hour 384. Requests are limited to the southern-border region
(`-118` to `-93` longitude and `23` to `35` latitude).

Install the additional raster and plotting packages:

```r
install.packages(c("terra", "ggplot2"))
```

Run the latest 00Z release from the repository root:

```powershell
Rscript R/run_gefs_release.R
```

To reproduce a particular model run, set the initialization date and cycle:

```powershell
$env:GEFS_DATE = "2026-11-02"
$env:GEFS_CYCLE = "0"
Rscript R/run_gefs_release.R
```

Raw responses are cached under `data/gefs/<run>/raw`, so an interrupted or
repeated release only downloads missing files. Complete local days are
aggregated in the Mountain time zone. The default operational products are:

- daily mean, maximum, and minimum 2 m temperature in degrees Fahrenheit
- daily accumulated precipitation in millimeters
- daily maximum 10 m sustained wind in miles per hour
- ensemble median and 10th and 90th percentiles
- probabilities of maximum temperature above 100 F, precipitation above
  25.4 mm, and sustained wind above 25 mph

Processed GeoTIFF files and CSV manifests remain under the run's `products`
directory. The release writes `output/gefs/forecast_map.html` and
`output/gefs/point_forecasts.png`. It also writes one interactive map per
complete forecast day under `output/gefs/daily_maps`; `forecast_map.html` is
the first complete day. The chart covers San Diego, Yuma, El Paso, Laredo, and
Brownsville across all available complete days.

Change bounds, timezone, thresholds, locations, members, or forecast hours by
calling `create_gefs_release()` directly after sourcing `R/gefs_forecast.R`.
NOMADS generally retains only recent runs and can be temporarily unavailable.
The cached GRIB2 files should therefore be retained for operational continuity;
long-term retrospective work should use the NOAA archive or public GEFS cloud
bucket.
