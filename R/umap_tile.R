umap_tile <- function(tile_loc, out_dir, scl = TRUE, ...){
  tile <- raster::stack(tile_loc)
  tile_pts <- raster::rasterToPoints(tile)
  cells <- raster::cellFromXY(tile, tile_pts[,1:2])
  in_dat <- tile_pts[, seq_len(raster::nlayers(tile)) + 2]
  if(isTRUE(scl)){in_dat <- scale(in_dat)}
  uwot_out <- uwot::umap(in_dat, ...)
  umap_1 <- umap_2 <- tile[[1]]
  values(umap_1)[cells] <- uwot_out[,1]
  values(umap_2)[cells] <- uwot_out[,2]
  umap_stack <- stack(umap_1, umap_2, tile)
  writeRaster(umap_stack, filename = file.path(out_dir, basename(tile_loc)))
}
