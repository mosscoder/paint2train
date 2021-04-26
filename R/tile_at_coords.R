#take matrix of coordinates (col. 1 x, col. 2 y) and a source image,
#then generate tiles of specified side length
#in units of image and put them in out_dir

tile_at_coords <-
  function(coords, img, len_side, out_dir, ncores = 1) {
    tile_ind <- seq_len(nrow(coords))
    
    ext_list <- replicate(extent(0, 0, 0, 0),
                          n = length(tile_ind))
    
    half_side <- len_side / 2
    
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
                                            paste0(names(ext_list)[x], '.tif')))
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
                                            paste0(names(ext_list)[x], '.tif')))
        },
        X = seq_along(tile_ind)
      )
    }
  }