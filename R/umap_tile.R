umap_tile <- function(tile, out_dir, scl = TRUE, ...){
  t <- raster::stack(tile)

  tile_pts <- raster::rasterToPoints(t) #%>% na.omit()
  cells <- raster::cellFromXY(t, tile_pts[,1:2])
  in_dat <- tile_pts[, seq_len(raster::nlayers(t)) + 2]
  if(isTRUE(scl)){in_dat <- scale(in_dat)}
  uwot_out <- uwot::umap(in_dat, n_components = 3, ...)
  umap_1 <- umap_2 <- umap_3 <- t[[1]]
  values(umap_1)[cells] <- uwot_out[,1]
  values(umap_2)[cells] <- uwot_out[,2]
  values(umap_3)[cells] <- uwot_out[,3]
  umap_stack <- raster::stack(umap_1, umap_2, umap_3, t)
  raster::writeRaster(umap_stack, filename = file.path(out_dir, basename(tile)), overwrite = T)
}
