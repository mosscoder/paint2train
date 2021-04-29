ndvi_msavi <- function(tile, r_band = 1, nir_band = 2){
  t <- raster::stack(tile)
  r <- t[[r_band]]
  nir <- t[[nir_band]]
  ndvi <- (nir - r) / (nir + r)
  msavi <- (2 * nir + 1 - sqrt ((2 * nir + 1)^2 - 8 * (nir - r))) / 2
  diffs_added <- raster::stack(t, ndvi, msavi)
  writeRaster(diffs_added, tile, overwrite = TRUE)
}