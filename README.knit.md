---
title: "paint2train documentation"
author: "Kyle Doherty"
date: "5/5/2021"
output:
  html_document:
    df_print: paged
---



<img src="https://github.com/mosscoder/paint2train/blob/main/images/intro_banner.png?raw=true" width="100%" />

## Background

The purpose of paint2train package is to facilitate imagery and spatial data labeling at the pixel level. These labels may in turn be used to train machine learning algorithms for tasks such as image segmentation. T

There are currently four primary functions:

* Generate tiles from larger contigous sources of multi-spectral imagery
* Preprocess tiles, generating normalized difference, edge detection, and neighborhood summary stats layers
* Reduce n layers in each preprocessed tile into 3 layers, using [UMAP](https://github.com/jlmelville/uwot)
* Paint pixels in a shiny app which identifies similar pixels in UMAP space and label them, saving labels in .tifs

## Installation 
The package may be installed via github. 

```r
devtools::install_github('mosscoder/paint2train')
```

## Preparatory Steps
Here we begin by downloading some a sample 4-band image. 

```r
library(paint2train)
library(ranger)

image_dir <- tempfile()
download.file(url = 'https://github.com/mosscoder/paint2train/blob/main/data/sample_4band.tif?raw=true', destfile = image_dir)

par(mfrow = c(2,1))  
plotRGB(stack(image_dir)[[1:3]], main = 'True color')
plotRGB(stack(image_dir)[[c(4,2,3)]], main = 'NIR false color')
mtext("15cm true and NIR false color imagery", side = 3, line = -1, outer = TRUE)
par(mfrow = c(1,1))  
```

<img src="https://github.com/mosscoder/paint2train/blob/main/images/plot_src_image.png?raw=true" width="100%" />

Next build the directories necessary to house tiles, pre-processed intermediaries, and labeled data. 

```r
tdir <- tempdir()
setwd(tdir) #where output directories will go
preproc_dir <- 'preproc_tiles' #dir for preprocessed tiles
umap_dir <- 'umap_tiles' #dir for UMAP output
lab_dir <- 'label_tiles' #dir for labeled .tif
pred_dir <- 'pred_dir' #a place to predict to new data 

lapply(FUN = function(x){dir.create(x)}, X = c(preproc_dir, umap_dir, lab_dir, pred_dir))
```

## Tiling and Pre-processing
Then define a 2-column matrix with coordinates corresponding to centroids at which to generate tiles. Specify tile size and a buffer to avoid edge effects during pre-processing neighborhood calculations. Also specify the number of cores used during preprocessing. Note that parallel processing will only work on Unix systems for the native functions in paint to train at this time. External functions, such as umap, will likely work on Windows systems, however, and greatly speed up the dimension reduction step.


```r
#some test coordinates
xcoords <- c(727495,
             727919)

ycoords <- c(5175339,
             5175408)

#bind into matrix
coord_mat <- cbind(xcoords, ycoords)

ls <- 30 #how big should the tiles be, this is the side length (in units of data, meters here)
buff <- 5  #buffer in units of data
pre_cores <- ifelse(.Platform$OS.type == 'unix', #how many cores to use for pre-processing
                detectCores() - 1,
                1) 
umap_cores <- detectCores() - 1 #how many cores to use for UMAP dimension reduction

#make 30m tiles with 5m buffer (to avoid edge effects during pre-processing)
tile_at_coords(coords = coord_mat, 
               len_side = ls, 
               buffer = buff,
               out_dir = preproc_dir,
               img = image_dir, 
               ncores = pre_cores)
```

<img src="https://github.com/mosscoder/paint2train/blob/main/images/focal_tiles_rgb.png?raw=true" width="100%" />

Now generate NDVI... 

```r
mclapply(
  FUN = ndvi_msavi,
  X = list.files(preproc_dir, full.names = T),
  mc.cores = pre_cores
)
```

<img src="https://github.com/mosscoder/paint2train/blob/main/images/ndvi.png?raw=true" width="100%" />

... and MSAVI values at tiles. 
<img src="https://github.com/mosscoder/paint2train/blob/main/images/msavi.png?raw=true" width="100%" />

Next, derive Sobel values (edge detection) for the first 3 PCA axes of data generated thus far.

```r
mclapply(
  FUN = sobel,
  X = list.files(preproc_dir, full.names = T),
  mc.cores = pre_cores
)
```
<img src="https://github.com/mosscoder/paint2train/blob/main/images/sobel.png?raw=true" width="100%" />

Next calculate mean and variance in 0.25, 0.5, and 1 meter neighborhoods for first three PCA axes of data generated up to this point.
The extents of these neighborhoods are a critical tuning parameter that will need to be paired with a particular dataset and modeling objective.


```r
neighborhoods <- c(0.25, 0.5, 1) #neighborhood radii in units of imagery
mclapply(
  FUN = mean_var,
  X = list.files(preproc_dir, full.names = T),
  f_width = neighborhoods, 
  mc.cores = pre_cores
)
```

As a final pre-processing step, remove the buffers from around each tile.

```r
mclapply(FUN = remove_buffer,
                   X = list.files(preproc_dir, full.names = T),
                   b = buff,
                   mc.cores = pre_cores)
```

Here are the outputs of the focal calculations described above for the first tile.
<img src="https://github.com/mosscoder/paint2train/blob/main/images/neighbs.png?raw=true" width="100%" />

## Dimension Reduction
Next we will reduce the pre-processed layers into three dimension with the UMAP algorithm. This will facilitate identification of similar pixels in the painting app by conducting non-linear transformations of the data and avoiding the curse of dimensionality. For details on this method, please refer to the [UWOT documentation](https://github.com/jlmelville/uwot).


```r
mclapply(FUN = umap_tile,
                   X = list.files(preproc_dir, full.names = T),
                   b = buff,
                   mc.cores = pre_cores)
```
We may compare the original RGB tiles with outcomes from pre-processing and dimension reduction, representing UMAP space with similarity colors. 
<img src="https://github.com/mosscoder/paint2train/blob/main/images/umap_example.png?raw=true" width="100%" />

## Labeling Data
Now we may label our data using the paint2train app. We first define the classes to label, assigning them an integer value, as well as a corresponding color palette to visualize labeled areas. 


```r
label_key <- list(Unknown = 0,
           `Not woody` = 1,
           `Woody` = 2)

pal <- c('royalblue', 
         'tan', 
         'green')
```
Now provide these lists, location of the UMAP output and label directory to the p2t function. 

```r
p2t(umap_dir = umap_dir, 
    label_dir = lab_dir, 
    label_key = label_key, 
    label_col = pal)
```

