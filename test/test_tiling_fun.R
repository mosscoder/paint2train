source('https://raw.githubusercontent.com/mosscoder/paint2train/main/R/tile_at_coords.R')
library(raster)

image_dir <- '/Volumes/mpg_data/rasters/High_Res_Mosaics/2011_cir_ranch_clip.tif' #replace with path to any imagery
output_dir <- '~/Scratch/test_tiles' #where to dump tiles

dir.create(output_dir)

#some test coordinates
xcoords <- c(728694.6722,
             728268.3661,
             727541.7446
             )

ycoords <- c(5177076.413,
             5176716.602,
             5176541.735)

coord_mat <- cbind(xcoords, ycoords)

tile_at_coords(coords = coord_mat, 
               len_side = 30, #how big should the tiles be, this is the side length
               out_dir = output_dir,
               img = image_dir, 
               ncores = 3) #parallel functionality likely only works on Unix systems
              