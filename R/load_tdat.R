#' @title Load and format imagery labels as a data.frame
#' @description Provide load_tdat with two directories: one containing spatial 
#' data produced in the pre-processing pipeline, the other containing data labels
#' generated with pt2. The function then assembles these data into a data.frame for model training. 
#' Parallel processing is available on Unix systems. 
#' 
#' @param preproc_dir Path to pre-processing tiles. 
#' @param label_dir Path to label data .tifs. 
#' @param ncores Cores to use for parallel implementation.

#' @return A data.frame in which the first column contains data labels and subsequent
#' columns contain potential predictors with order corresponding to the band 
#' order of tiles in the preproc_dir. 
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
#' lab_dir <- 'label_tiles'
#' dir.create(preproc_dir)
#' dir.create(umap_dir)
#' dir.create(lab_dir)
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
#' label_key <- list(Unknown = 0,
#'         `Not woody` = 1,
#'         `Woody` = 2)
#' #Establish the color  for each class for app visualization
#' pal <- c('royalblue',
#'         'tan',
#'         'green')
#'
#'# Start the app, note that work will be saved every time the 
#'# label, filter, fill buttons are clicked within the app.
#'# Prior work saved in the label_dir will be loaded to resume labeling
#'p2t(umap_dir = umap_dir, 
#'    label_dir = lab_dir, 
#'    label_key = label_key, 
#'    label_col = pal)
#' 
#' train_dat <- load_tdat(preproc_dir = preproc_dir, label_dir = lab_dir, ncores = cores)
#' 
#' @export
load_tdat <- function(preproc_dir, label_dir, ncores = 1)
  do.call(rbind,
          parallel::mclapply(
            FUN = function(x) {
              labels <- raster::raster(file.path(label_dir, x))
              predictors <- raster::stack(file.path(preproc_dir, x))
              train_stack <- raster::stack(labels, predictors)
              train_vals <- raster::getValues(train_stack)
              pred_names <-
                c('label', paste0('X_', seq_len(raster::nlayers(train_stack) - 1)))
              colnames(train_vals) <- pred_names
              return(as.data.frame(train_vals))
            },
            X = list.files(label_dir),
            mc.cores = ncores
          ))