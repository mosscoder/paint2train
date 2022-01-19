#' @title Perform UMAP dimension reduction on raster data
#'
#' @description umap_tile takes a multi-band raster stack and reduces it to three components via the UMAP algorithm
#' as implemented in UWOT package. Tuning parameters may be passed to umap() via ... 
#' 
#' @param tile File path to a multi-band raster stack. 
#' @param out_dir Directory into which rasterized UMAP data are stored. 
#' @param scl Whether to z-score input data. 
#' @param... Additional arguments to pass to umap(). 
#' 
#' @examples 
#' library(paint2train)
#' 
#' image_dir <- tempfile()
#' image_url <- 'https://storage.googleapis.com/mpgranch_data/sample_4band.tif'
#' download.file(url = image_url, destfile = image_dir)
#' tdir <- tempdir()
#' setwd(tdir) 
#' preproc_dir <- 'preproc_tiles'
#' umap_dir <- 'umap_tiles'
#' dir.create(preproc_dir)
#' dir.create(umap_dir)
#' 
#' #some test coordinates
#' xcoords <- c(727495,
#'              727919)
#' 
#' ycoords <- c(5175339,
#'              5175408)
#'
#' coord_mat <- cbind(xcoords, ycoords)
#'
#' ls <- 30 #how big should the tiles be, this is the side length (in units of data, meters here)
#' buff <- 5  #buffer in native units of CRS
#' cores <- ifelse(.Platform$OS.type == 'unix', #how many cores to use for preprocessing
#'                    parallel::detectCores() - 1,
#'                    1) 
#' umap_cores <- parallel::detectCores() - 1                  
#'    
#'                    
#' tile_at_coords(coords = coord_mat,
#'  len_side = ls,
#'  buffer = buff,
#'  out_dir = preproc_dir,
#'  img = image_dir,
#'  ncores = cores)
#'
#' preproc_pipeline <- function(t, fs, b){
#'  ndvi_msavi(tile = t, r_band = 1, nir_band = 4)
#'  sobel(t, axes = 3, fill_na = TRUE)
#'  mean_var(t, axes = 3, f_width = fs, fill_na = TRUE)
#'  remove_buffer(tile = t, b = b)
#' }
#' 
#' targ_tiles <- list.files(preproc_dir, full.names = TRUE)
#' 
#' mclapply(FUN = preproc_pipeline, 
#'  X = targ_tiles, 
#'  mc.cores = cores, 
#'  fs = c(0.5, 1),
#'  b = buff)
#'  
#' lapply(FUN = umap_tile,
#'  X = targ_tiles,
#'  out_dir = umap_dir,
#'  n_threads = umap_cores, #args passed to umap
#'  n_sgd_threads = umap_cores, #args passed to umap
#' )
#' 
#' par(mfrow = c(2,2))
#' for(i in seq_along(xcoords)) {
#'  plotRGB(stack(list.files(preproc_dir, full.names = T)[i])[[1:3]])
#' }
#' for (i in seq_along(xcoords)) {
#'  umap_st <- stack(list.files(umap_dir, full.names = T)[i])[[1:3]]
#'  values(umap_st) <- rescale(getValues(umap_st))
#'  plotRGB(umap_st, scale = 1)
#' }
#' par(mfrow = c(1, 1))
#' @export
umap_tile <- function(tile, out_dir, scl = TRUE, ...){
  t <- raster::stack(tile)
  tile_pts <- raster::rasterToPoints(t) 
  cells <- raster::cellFromXY(t, tile_pts[,1:2])
  in_dat <- tile_pts[, seq_len(raster::nlayers(t)) + 2]
  if(isTRUE(scl)){in_dat <- scale(in_dat)}
  uwot_out <- uwot::umap(in_dat, n_components = 3, ...)
  umap_1 <- umap_2 <- umap_3 <- t[[1]]
  raster::values(umap_1)[cells] <- uwot_out[,1]
  raster::values(umap_2)[cells] <- uwot_out[,2]
  raster::values(umap_3)[cells] <- uwot_out[,3]
  umap_stack <- raster::stack(umap_1, umap_2, umap_3, t)
  raster::writeRaster(umap_stack, filename = file.path(out_dir, basename(tile)), overwrite = T)
}
