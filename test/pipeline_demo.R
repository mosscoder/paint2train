# source('https://raw.githubusercontent.com/mosscoder/paint2train/main/R/tile_at_coords.R')
# source('https://raw.githubusercontent.com/mosscoder/paint2train/main/R/umap_tile.R')
# source('https://raw.githubusercontent.com/mosscoder/paint2train/main/R/preproc_funs.R')
source('~/mpgPostdoc/projects/paint2train/R/tile_at_coords.R')
source('~/mpgPostdoc/projects/paint2train/R/preproc_funs.R')
source('~/mpgPostdoc/projects/paint2train/R/umap_tile.R')
source('~/mpgPostdoc/projects/paint2train/R/p2t.R')
library(raster)
library(tidyverse)

image_dir <- '/Volumes/mpg_data/rasters/High_Res_Mosaics/2020_4band_clip_NAD83.tif' #replace with path to any imagery
setwd('~/Scratch') #where output directories will go
preproc_dir <- 'preproc_tiles' #dir for preprocessed tiles
umap_dir <- 'umap_tiles' #dir for umap output
lab_dir <- 'label_tiles' #dir for labeled tifs

lapply(FUN = function(x){dir.create(x)}, X = c( preproc_dir, umap_dir, lab_dir))

#some test coordinates
xcoords <- c(727495,
             727919)

ycoords <- c(5175339,
             5175408)

#bind into matrix
coord_mat <- cbind(xcoords, ycoords)

ls <- 30 #how big should the tiles be, this is the side length (in units of data, meters here)
buff <- 5  #buffer in units of data
cores <- 3 #how many processors to use, not tested outside of Unix systems

#make 30m tiles with 5m buffer (to avoid edge effects during pre-processing)
tile_at_coords(coords = coord_mat, 
               len_side = ls, 
               buffer = buff,
               out_dir = preproc_dir,
               img = image_dir, 
               ncores = cores)

#show tiling outcome
par(mfrow = c(1,2))              
for(i in seq_along(xcoords)){plotRGB(stack(list.files(preproc_dir, full.names = T)[i])[[1:3]])}
par(mfrow = c(1,1))

#derive additional features, NDVI and MSAVI
parallel::mclapply(FUN = ndvi_msavi,
                   X = list.files(preproc_dir, full.names = T),
                   mc.cores = cores)

#show ndvi
par(mfrow = c(1,2))              
for(i in seq_along(xcoords)){plot(stack(list.files(preproc_dir, full.names = T)[i])[[5]])}
par(mfrow = c(1,1))

#show msavi
par(mfrow = c(1,3))              
for(i in seq_along(xcoords)){plot(stack(list.files(preproc_dir, full.names = T)[i])[[6]])}
par(mfrow = c(1,1))

parallel::mclapply(FUN = sobel,
                   X = list.files(preproc_dir, full.names = T),
                   mc.cores = cores)

#show edges for first pca axis
par(mfrow = c(1,2))              
for(i in seq_along(xcoords)){plot(stack(list.files(preproc_dir, full.names = T)[i])[[7]], col = gray.colors(100))}
par(mfrow = c(1,1))

parallel::mclapply(FUN = mean_var,
                   X = list.files(preproc_dir, full.names = T),
                   f_width = c(0.25, 1, 3), #calculate mean and variance in 0.25m, 1m, and 3m neighborhoods
                   mc.cores = cores)

#show mean first pca axis
par(mfrow = c(1,2))              
for(i in seq_along(xcoords)){plot(stack(list.files(preproc_dir, full.names = T)[i])[[10]])}
par(mfrow = c(1,1))

#show variance first pca axis
par(mfrow = c(1,2))              
for(i in seq_along(xcoords)){plot(stack(list.files(preproc_dir, full.names = T)[i])[[13]])}
par(mfrow = c(1,1))

#remove bufffers after preprocessing
parallel::mclapply(FUN = remove_buffer,
                   X = list.files(preproc_dir, full.names = T),
                   b = buff,
                   mc.cores = cores)

#Plot an example of all layers after preprocessing
example_img <- stack(list.files(preproc_dir, full.names = T)[1])
plot(example_img)

#Reduce data to three umap dimensions
lapply(FUN = umap_tile,
                   X = list.files(preproc_dir, full.names = TRUE),
                   out_dir = umap_dir,
                   n_threads = 25L,
                   n_sgd_threads = 25L)

#Display false color representation of three UMAP dimensions
par(mfrow = c(1,2))              
for(i in seq_along(xcoords)){
  umap_st <- stack(list.files(umap_dir, full.names = T)[i])[[1:3]]
  values(umap_st) <- scales::rescale(getValues(umap_st))
  plotRGB(umap_st, scale = 1)
  }
par(mfrow = c(1,1))


lk <- list(Unknown = 0,
           `Not canopy` = 1,
           `Woody canopy` = 2)
lc <- c('royalblue', 'tan', 'green')

p2t(umap_dir = umap_dir, label_dir = lab_dir, label_key = lk, label_col = lc)




