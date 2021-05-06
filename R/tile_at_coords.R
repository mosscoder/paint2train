#' @title Generate tiles from imagery and spatial data
#' @description Accepts a matrix of coordinates and a path to a multi-band raster.
#' Generates tiles of specified side length
#' in CRS units of the data with an optional buffer to avoid edge effects in subsequent preocessing. 
#' Save tiles to an output directory for further preprocessing.
#' 
#' 
#' @param coords A two column matrix containing x and y coordinates.
#' @param img The path to the source raster stack. 
#' @param len_side Desired side length of tiles in img CRS units.  
#' @param out_dir Directory in which to place tile output.
#' @param buffer Distance in native CRS units to buffer around tiles.
#' @param ncores The number of cores to use for parallel tiling. 
#' @return None. Tiles are saved to out_dir. 
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
#' #show tiling outcome
#' par(mfrow = c(1,2))              
#' for(i in seq_along(xcoords)){plotRGB(stack(list.files(preproc_dir, full.names = T)[i])[[1:3]])}
#' par(mfrow = c(1,1))
#'  
#' @export
tile_at_coords <-
  function(coords, img, len_side, out_dir, buffer = 0, ncores = 1) {
    tile_ind <- seq_len(nrow(coords))
    
    ext_list <- replicate(raster::extent(0, 0, 0, 0),
                          n = length(tile_ind))
    
    half_side <- len_side / 2
    if(buffer > 0){ half_side <- half_side + buffer }
    
    for (i in tile_ind) {
      ext_list[[i]] <- raster::extent(coords[i, 1] - half_side,
                                      coords[i, 1] + half_side,
                                      coords[i, 2] - half_side,
                                      coords[i, 2] + half_side)
    }
    
    names(ext_list) <- paste(
      stringr::str_pad(tile_ind,
                       width = floor(log10(length(
                         tile_ind
                       ))) + 2,
                       pad = '0'),
      round(coords[, 1]),
      round(coords[, 2]),
      sep = '_'
      
    )
    
    if (ncores > 1) {
      parallel::mclapply(
        FUN = function(x) {
          raster::crop(raster::stack(img),
                       ext_list[[x]],
                       filename = file.path(out_dir,
                                            paste0(names(ext_list)[x], '.tif')), 
                       overwrite = TRUE)
        },
        X = seq_along(tile_ind),
        mc.cores = ncores
      )
    } else {
      lapply(
        FUN = function(x) {
          raster::crop(raster::stack(img),
                       ext_list[[x]],
                       filename = file.path(out_dir,
                                            paste0(names(ext_list)[x], '.tif')), 
                       overwrite = TRUE)
        },
        X = seq_along(tile_ind)
      )
    }
  }