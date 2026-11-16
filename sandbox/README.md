# Smoothed GEFS map sandbox

From the repository root, render the cached November 12 forecast with bilinear
raster smoothing:

```powershell
Rscript sandbox/create_smoothed_gefs_map.R
```

The script reads the `20261102_00z` product manifest and writes
the self-contained `sandbox/output/2026-11-12-smoothed.html`. It passes
`smooth_factor = 4L` to
`create_gefs_map()`, which upsamples continuous layers with
`terra::disagg(..., method = "bilinear")` and uses bilinear reprojection in
Leaflet. Wind direction remains unsmoothed because linear interpolation is not
valid across the 0/360-degree boundary.