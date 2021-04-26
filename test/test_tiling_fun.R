source('~/mpgPostdoc/projects/paint2train/R/tile_at_coords.R')

image_dir <- '/Volumes/mpg_data/rasters/High_Res_Mosaics/2011_cir_ranch_clip.tif'
output_dir <- '~/Scratch/test_tiles'
dir.create(output_dir)

xcoords <- c(728694.6722,
             728268.3661,
             727541.7446
             )

ycoords <- c(5177076.413,
             5176716.602,
             5176541.735)

coord_mat <- cbind(xcoords, ycoords)

tile_at_coords(coords = coord_mat, 
               len_side = 30,
               out_dir = scratch_dir,
               img = img_dir, 
               ncores = 3)
              