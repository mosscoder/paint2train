#' @title Calculate NDVI and MSAVI
#' @description Takes a multi-band raster stack and calculates NDVI and MSAVI vegetation indices. 
#' These indices are appended as the last two bands of the input tile. 
#' @param tile File path to a mulit-band raster stack.
#' @param r_band Stack index of the red band.
#' @param nir_band Stack index of the near-infrared band.
#' @return None. Appends bands to tile and overwrites appended stack to tile file path. 
#' 
#' @examples 
#' library(paint2train)
#' 
#' image_dir <- tempfile()
#' download.file(url = 'https://github.com/mosscoder/paint2train/blob/main/data/sample_4band.tif?raw=true', destfile = image_dir)
#' tdir <- tempdir()
#' setwd(tdir) 
#' preproc_dir <- 'preproc_tiles'
#' dir.create(preproc_dir)
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
#'                    detectCores() - 1,
#'                    1) 
#'                    
#' tile_at_coords(coords = coord_mat,
#'  len_side = ls,
#'  buffer = buff,
#'  out_dir = preproc_dir,
#'  img = image_dir,
#'  ncores = cores)
#'  
#' t <- list.files(preproc_dir, full.names = TRUE)[1]
#' ndvi_msavi(tile = t, r_band = 1, nir_band = 4)
#'  
#' t_ndvi <- raster(t, band = 5)
#' t_msavi <- raster(t, band = 6)
#' plot(t_ndvi)
#' plot(t_msavi)
#' @export
ndvi_msavi <- function(tile, r_band = 1, nir_band = 4){
  t <- raster::stack(tile)
  r <- t[[r_band]]
  nir <- t[[nir_band]]
  ndvi <- (nir - r) / (nir + r)
  msavi <- (2 * nir + 1 - sqrt ((2 * nir + 1)^2 - 8 * (nir - r))) / 2
  diffs_added <- raster::stack(t, ndvi, msavi)
  raster::writeRaster(diffs_added, tile, overwrite = TRUE)
}

#' @title Apply Sobel filter
#' @description sobel takes a multi-band raster stack, conducts principal components analysis, then 
#' applies a Sobel filter to n PCA axes and appends these n bands to the tile. 
#' 
#' @param tile File path to a mulit-band raster stack. 
#' @param axes How many PCA axes to retain and filter.
#' @param fill_na Whether to impute NA cells with mean of neighboring cells.
#' @return None. Appends n bands to tile and overwrites appended stack to tile file path. 
#' 
#' @examples 
#' library(paint2train)
#' 
#' image_dir <- tempfile()
#' download.file(url = 'https://github.com/mosscoder/paint2train/blob/main/data/sample_4band.tif?raw=true', destfile = image_dir)
#' tdir <- tempdir()
#' setwd(tdir) 
#' preproc_dir <- 'preproc_tiles'
#' dir.create(preproc_dir)
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
#'                    detectCores() - 1,
#'                    1) 
#'                    
#' tile_at_coords(coords = coord_mat,
#'  len_side = ls,
#'  buffer = buff,
#'  out_dir = preproc_dir,
#'  img = image_dir,
#'  ncores = cores)
#'  
#' t <- list.files(preproc_dir, full.names = TRUE)[1]
#' ndvi_msavi(tile = t, r_band = 1, nir_band = 4)
#' sobel(t, axes = 3, fill_na = TRUE)
#' t_sobels <- stack(t)[[7:9]]
#' plot(t_sobels)

#' @export
sobel <- function(tile, axes = 3, fill_na = TRUE){
  t <- raster::stack(tile)
  
  if(isTRUE(fill_na)){
    for(i in seq_len(raster::nlayers(t))){
      t[[i]] <- raster::focal(t[[i]], matrix(1,3,3), fun="mean", na.rm=TRUE, NAonly=TRUE, pad=TRUE) 
    }
  }
  
  vals <- raster::getValues(t)
  pca <- stats::prcomp(vals, center = TRUE, scale. = TRUE)$x[,seq_len(axes), drop = FALSE]
  sobel_stack <- pca_stack <- t[[seq_len(axes)]]
  
  sob_vec <- c(1,2,1,0,0,0,-1,-2,-1)
  
  sb_x_mat <- matrix(data = sob_vec,
                     ncol = 3, 
                     nrow = 3)
  sb_y_mat <- t(sb_x_mat)
  
  for(i in seq_len(axes)){
    raster::values(pca_stack[[i]]) <- pca[, i, drop = FALSE]
    sx <- raster::focal(pca_stack[[i]], w = sb_x_mat, na.rm = TRUE)
    sy <- raster::focal(pca_stack[[i]], w = sb_y_mat, na.rm = TRUE)
    sobel_stack[[i]] <- sqrt(sx^2 + sy^2)
  }
  
  sobel_added <- raster::stack(t, sobel_stack)
  raster::writeRaster(sobel_added, tile, overwrite = TRUE)
}

#' @title Calculate mean and variance for a target neighborhood
#' @description mean_var takes a multi-band raster stack, conducts principal components analysis, 
#' then applies mean and variance calculations within a specified neighborhood or list of neighborhoods for 
#' the range of retained PCA axes.
#' 
#' @param tile File path to a mulit-band raster stack. 
#' @param axes How many PCA axes to retain for neighborhood calculations.
#' @param f_width Focal neighborhood radius or radii (if a list) in which to perform calculations. 
#' Units correspond to that of the tile CRS.
#' @param fill_na Whether to impute NA cells with mean of neighboring cells.
#' @return None. Appends i PCA axes by j f_widths for both mean and variance 
#' (axes X neighborhoods X 2) to tile and overwrites appended stack to tile file path. 
#' Appended bands are sorted hierarchically by summary type (mean first), 
#' then neighborhood, and finally PCA axis. 
#'
#' @examples 
#' library(paint2train)
#' 
#' image_dir <- tempfile()
#' download.file(url = 'https://github.com/mosscoder/paint2train/blob/main/data/sample_4band.tif?raw=true', destfile = image_dir)
#' tdir <- tempdir()
#' setwd(tdir) 
#' preproc_dir <- 'preproc_tiles'
#' dir.create(preproc_dir)
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
#'                    detectCores() - 1,
#'                    1) 
#'                    
#' tile_at_coords(coords = coord_mat,
#'  len_side = ls,
#'  buffer = buff,
#'  out_dir = preproc_dir,
#'  img = image_dir,
#'  ncores = cores)
#'  
#' t <- list.files(preproc_dir, full.names = TRUE)[1]
#' ndvi_msavi(tile = t, r_band = 1, nir_band = 4)
#' sobel(t, axes = 3, fill_na = TRUE)
#' 
#' fs <- c(0.5, 1)
#' 
#' mean_var(t, axes = 3, f_width = fs, fill_na = TRUE)
#' t_full <- stack(t)
#' t_mv <- t_full[[10:nlayers(t_full)]]
#' 
#' #mean_var output
#' plot(t_mv)
#' 
#' @export  
mean_var <- function(tile, axes = 3, f_width, fill_na = TRUE){
  t <- raster::stack(tile)
  
  if(isTRUE(fill_na)){
    for(i in seq_len(raster::nlayers(t))){
      t[[i]] <- raster::focal(t[[i]], matrix(1,3,3), fun="mean", na.rm=TRUE, NAonly=TRUE, pad=TRUE) 
    }
  }
  
  vals <- raster::getValues(t)
  
  pca <- stats::prcomp(vals, center = TRUE, scale. = TRUE)$x[,seq_len(axes), drop = FALSE]
  pca_stack <- t[[seq_len(axes)]]
  for (i in seq_len(axes)) {
    raster::values(pca_stack[[i]]) <- pca[, i, drop = FALSE]
  }
  
  f_grid <- expand.grid(c('mean','var'),
                        f_width, 
                        seq_len(axes)) %>%
    as.data.frame()
  colnames(f_grid) <- c('fun','width','pca_axis')
  f_grid <- f_grid[order(f_grid[,1], 
                         f_grid[,2],
                         f_grid[,3]),]
  
  summary_stack <- replicate(t[[1]], n = nrow(f_grid))
  
  for(i in seq_len(nrow(f_grid))){
    mat <- raster::focalWeight(t, f_grid$width[i], type=c('circle'))
    
    if (f_grid$fun[i] == 'mean') {
      summary_stack[[i]] <- raster::focal(
        pca_stack[[f_grid$pca_axis[i]]],
        w = mat,
        fun = base::mean,
        na.rm = TRUE,
        pad = TRUE
      )
    }
    
    if (f_grid$fun[i] == 'var') {
      summary_stack[[i]] <- raster::focal(
        pca_stack[[f_grid$pca_axis[i]]],
        w = mat,
        fun = stats::var,
        na.rm = TRUE,
        pad = TRUE
      )
    }
    
  }
  
  mv_added <- raster::stack(t, raster::stack(summary_stack))
  raster::writeRaster(mv_added, tile, overwrite = TRUE)
}

#' @title Remove tile buffers
#' @description Removes buffers if included with tile_at_coords() after pre-processing. 
#' 
#' @param tile File path to a mulit-band raster stack. 
#' @param b Buffer width in tile CRS units. 
#' @return None. Overwrites tile with buffers removed. 
#'
#' @examples 
#' library(paint2train)
#' 
#' image_dir <- tempfile()
#' download.file(url = 'https://github.com/mosscoder/paint2train/blob/main/data/sample_4band.tif?raw=true', destfile = image_dir)
#' tdir <- tempdir()
#' setwd(tdir) 
#' preproc_dir <- 'preproc_tiles'
#' dir.create(preproc_dir)
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
#'                    detectCores() - 1,
#'                    1) 
#'                    
#' tile_at_coords(coords = coord_mat,
#'  len_side = ls,
#'  buffer = buff,
#'  out_dir = preproc_dir,
#'  img = image_dir,
#'  ncores = cores)
#'  
#' t <- list.files(preproc_dir, full.names = TRUE)[1]
#' ndvi_msavi(tile = t, r_band = 1, nir_band = 4)
#' sobel(t, axes = 3, fill_na = TRUE)
#' fs <- c(0.5, 1)
#' mean_var(t, axes = 3, f_width = fs, fill_na = TRUE)
#' 
#'remove_buffer(tile = t, b = buff)
#' 
#' @export  
remove_buffer <- function(tile, b){
  t <- raster::stack(tile)
  buff_e <- raster::extent(t)
  no_buff_e <- raster::extent(buff_e[1] - b,
                      buff_e[2] + b,
                      buff_e[3] - b,
                      buff_e[4] + b)
  crp <- raster::crop(t, no_buff_e)
  crp <- raster::setExtent(crp, no_buff_e)
  raster::writeRaster(crp, tile, overwrite = TRUE)
}

