source('https://raw.githubusercontent.com/mosscoder/paint2train/main/R/tile_at_coords.R')
source('https://raw.githubusercontent.com/mosscoder/paint2train/main/R/preproc_funs.R')
#source('~/mpgPostdoc/projects/paint2train/R/tile_at_coords.R')
library(raster)

image_dir <- '/Volumes/mpg_data/rasters/High_Res_Mosaics/2020_4band_clip_NAD83.tif' #replace with path to any imagery
setwd('~/Scratch') #where output directories will go
raw_dir <- 'raw_tiles' #dir for raw tiles
preproc_dir <- 'preproc_tiles' #dir for preprocessed tiles
umap_dir <- 'umap_tiles' #dir for umap output
lab_dir <- 'label_tiles' #dir for labeled tifs

lapply(FUN = function(x){dir.create(x)}, X = c(raw_dir, preproc_dir, umap_dir, lab_dir))

#some test coordinates
xcoords <- c(728694.6722,
             728268.3661,
             727541.7446
             )

ycoords <- c(5177076.413,
             5176716.602,
             5176541.735)

#bind into matrix
coord_mat <- cbind(xcoords, ycoords)

ls <- 30 #how big should the tiles be, this is the side length (in units of data, meters here)
buff <- 5  #buffer in units of data
cores <- 3 #how many processors to use, not tested outside of Unix systems

#make 30m tiles with 5m buffer (to avoid edge effects during pre-processing)
tile_at_coords(coords = coord_mat, 
               len_side = ls, 
               buffer = buff,
               out_dir = raw_dir,
               img = image_dir, 
               ncores = cores)

#show tiling outcome
par(mfrow = c(1,3))              
for(i in seq_along(xcoords)){plotRGB(stack(list.files(raw_dir, full.names = T)[i])[[1:3]])}
par(mfrow = c(1,1))

#derive additional features




