ndvi_msavi <- function(tile, r_band = 1, nir_band = 4){
  t <- raster::stack(tile)
  r <- t[[r_band]]
  nir <- t[[nir_band]]
  ndvi <- (nir - r) / (nir + r)
  msavi <- (2 * nir + 1 - sqrt ((2 * nir + 1)^2 - 8 * (nir - r))) / 2
  diffs_added <- raster::stack(t, ndvi, msavi)
  raster::writeRaster(diffs_added, tile, overwrite = TRUE)
}

sobel <- function(tile, axes = 3, fill_na = TRUE, ...){
  t <- raster::stack(tile)
  
  if(isTRUE(fill_na)){
    for(i in seq_len(raster::nlayers(t))){
      t[[i]] <- focal(t[[i]], matrix(1,3,3), fun="mean", na.rm=TRUE, NAonly=TRUE, pad=TRUE) 
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
    sx <- focal(pca_stack[[i]], w = sb_x_mat, na.rm = TRUE)
    sy <- focal(pca_stack[[i]], w = sb_y_mat, na.rm = TRUE)
    sobel_stack[[i]] <- sqrt(sx^2 + sy^2)
  }
  
  sobel_added <- raster::stack(t, sobel_stack)
  raster::writeRaster(sobel_added, tile, overwrite = TRUE)
}

mean_var <- function(tile, axes = 3, f_width, fill_na = TRUE, ...){
  t <- raster::stack(tile)
  
  if(isTRUE(fill_na)){
    for(i in seq_len(raster::nlayers(t))){
      t[[i]] <- focal(t[[i]], matrix(1,3,3), fun="mean", na.rm=TRUE, NAonly=TRUE, pad=TRUE) 
    }
  }
  
  vals <- raster::getValues(t)
  
  pca <- stats::prcomp(vals, center = TRUE, scale. = TRUE)$x[,seq_len(axes), drop = FALSE]
  pca_stack <- t[[seq_len(axes)]]
  for (i in seq_len(axes)) {
    raster::values(pca_stack[[i]]) <- pca[, i, drop = FALSE]
  }
  f_grid <- expand.grid(width = f_width, 
                        fun = c('mean','var'),
                        pca_axis = seq_len(axes)) %>%
    as.data.frame() %>%
    arrange(fun, width, pca_axis)
  
  summary_stack <- replicate(t[[1]], n = nrow(f_grid))
  
  for(i in seq_len(nrow(f_grid))){
    mat <- focalWeight(t, f_grid$width[i], type=c('circle'))
    
    if (f_grid$fun[i] == 'mean') {
      summary_stack[[i]] <- focal(
        pca_stack[[f_grid$pca_axis[i]]],
        w = mat,
        fun = mean,
        na.rm = TRUE,
        pad = TRUE
      )
    }
    
    if (f_grid$fun[i] == 'var') {
      summary_stack[[i]] <- focal(
        pca_stack[[f_grid$pca_axis[i]]],
        w = mat,
        fun = var,
        na.rm = TRUE,
        pad = TRUE
      )
    }
    
  }
  
  mv_added <- raster::stack(t, stack(summary_stack))
  raster::writeRaster(mv_added, tile, overwrite = TRUE)
}

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

