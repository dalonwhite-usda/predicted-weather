# predicted-weather

Build an interactive Leaflet map of forward temperature and precipitation
forecasts for operational decision making.

## Inputs

Download the temperature and precipitation KMZ forecasts from the
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

From the repository root:

```r
source("R/create_forecast_map.R")

forecast_files <- c(
  temperature_6_10_day = "data/temperature_6_10_day.kmz",
  temperature_8_14_day = "data/temperature_8_14_day.kmz",
  temperature_3_4_week = "data/temperature_3_4_week.kmz",
  temperature_30_90_day = "data/temperature_30_90_day.kmz",
  precipitation_6_10_day = "data/precipitation_6_10_day.kmz",
  precipitation_8_14_day = "data/precipitation_8_14_day.kmz",
  precipitation_3_4_week = "data/precipitation_3_4_week.kmz",
  precipitation_30_90_day = "data/precipitation_30_90_day.kmz"
)

create_forecast_map(
  forecast_files,
  dispersal_polygon = "data/dispersal_polygon.geojson",
  output_file = "output/forecast_map.html"
)
```

Open `output/forecast_map.html` in a browser. The layer control can independently
show or hide every forecast and the dispersal polygon. The two 6–10 day layers
are shown initially; the other forecasts remain available in the control.
Forecast features are shaded by above/below-normal category and probability,
and hovering shows the feature label from the CPC file. Omit
`dispersal_polygon` when no polygon is available.

[cpc]: https://www.cpc.ncep.noaa.gov/products/GIS/GIS_DATA/
