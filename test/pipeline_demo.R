devtools::install_github('mosscoder/paint2train')

library(paint2train)
library(ranger)

#download some sample 4-band imagery
image_dir <- tempfile()
download.file(url = 'https://github.com/mosscoder/paint2train/blob/main/data/sample_4band.tif?raw=true', destfile = image_dir)

par(mfrow = c(2,1))  
plotRGB(stack(image_dir)[[1:3]], main = 'True color')
plotRGB(stack(image_dir)[[c(4,2,3)]], main = 'NIR false color')
mtext("15cm true and NIR false color imagery", side = 3, line = -1, outer = TRUE)
par(mfrow = c(1,1))  

tdir <- tempdir()
setwd(tdir) #where output directories will go
file.remove(list.files(tdir, full.names = TRUE, recursive = TRUE)) 
preproc_dir <- 'preproc_tiles' #dir for preprocessed tiles
umap_dir <- 'umap_tiles' #dir for umap output
lab_dir <- 'label_tiles' #dir for labeled tifs

lapply(FUN = function(x){dir.create(x)}, X = c(preproc_dir, umap_dir, lab_dir))

#some test coordinates
xcoords <- c(727495,
             727919)

ycoords <- c(5175339,
             5175408)

#bind into matrix
coord_mat <- cbind(xcoords, ycoords)

ls <- 30 #how big should the tiles be, this is the side length (in units of data, meters here)
buff <- 5  #buffer in units of data
pre_cores <- ifelse(.Platform$OS.type == 'unix', #how many cores to use for preprocessing
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

#show tiling outcome
par(mfrow = c(1,2))              
for(i in seq_along(xcoords)){plotRGB(stack(list.files(preproc_dir, full.names = T)[i])[[1:3]])}
mtext("The two tiles we will focus on", side = 3, line = -1, outer = TRUE)
par(mfrow = c(1,1))

#derive additional features, NDVI and MSAVI
mclapply(
  FUN = ndvi_msavi,
  X = list.files(preproc_dir, full.names = T),
  mc.cores = pre_cores
)

#show ndvi
par(mfrow = c(1,2))              
for(i in seq_along(xcoords)){plot(stack(list.files(preproc_dir, full.names = T)[i])[[5]])}
mtext("NDVI", side = 3, line = -1, outer = TRUE)
par(mfrow = c(1,1))

#show msavi
par(mfrow = c(1,2))              
for(i in seq_along(xcoords)){plot(stack(list.files(preproc_dir, full.names = T)[i])[[6]])}
mtext("MSAVI", side = 3, line = -1, outer = TRUE)
par(mfrow = c(1,1))

#perform edge detection on the first 3 PCA axes of initial 4 bands, ndvi, and msavi
mclapply(
  FUN = sobel,
  X = list.files(preproc_dir, full.names = T),
  mc.cores = pre_cores
)

#Show edges for first PCA axis
par(mfrow = c(1,2))              
for(i in seq_along(xcoords)){plot(stack(list.files(preproc_dir, full.names = T)[i])[[7]], col = gray.colors(100))}
mtext("PCA axis 1 Sobel values", side = 3, line = -1, outer = TRUE)
par(mfrow = c(1,1))

#Next calculate mean and variance in 0.25, 0.5, and 1 meter neighborhoods
#for first three PCA axes of data generated up to this point
#The extents of these neighborhoods are a critical tuning parameter that
#will need to be paired with a particular dataset and training objective

neighborhoods <- c(0.25, 0.5, 1)
mclapply(
  FUN = mean_var,
  X = list.files(preproc_dir, full.names = T),
  f_width = neighborhoods, #neighborhood radius in units of imagery
  mc.cores = pre_cores
)

#remove bufffers after preprocessing
mclapply(FUN = remove_buffer,
                   X = list.files(preproc_dir, full.names = T),
                   b = buff,
                   mc.cores = pre_cores)

# Plot neighborhood calculations after preprocessing for first tile
example_img <- stack(list.files(preproc_dir, full.names = T)[1])
neighb_inds <- 10:nlayers(example_img)
plot(example_img[[neighb_inds]], main = 'Output from mean and variance neighborhood calcs')

# Reduce data to three umap dimensions, 
# Note you can pass in further arguments to ?uwot::umap
lapply(FUN = umap_tile,
                   X = list.files(preproc_dir, full.names = TRUE),
                   out_dir = umap_dir,
                   n_threads = umap_cores,
                   n_sgd_threads = umap_cores)

#Display false color representation of three UMAP dimensions compared with RGB channels
par(mfrow = c(2,2))
for(i in seq_along(xcoords)){plotRGB(stack(list.files(preproc_dir, full.names = T)[i])[[1:3]])}
for(i in seq_along(xcoords)){
  umap_st <- stack(list.files(umap_dir, full.names = T)[i])[[1:3]]
  values(umap_st) <- rescale(getValues(umap_st))
  plotRGB(umap_st, scale = 1)
  mtext("Original tiles and corresponding UMAP values\nrepresented in similarity colors", side = 3, line = -3, outer = TRUE, col = 'lightgreen')
  }
par(mfrow = c(1,1))

# Define our classes of interest and corresponding integer values 
# to which they will be assigned in the .tif output
label_key <- list(Unknown = 0,
           `Not woody` = 1,
           `Woody` = 2)
#Establish the color  for each class for app visualization
pal <- c('royalblue', 
         'tan', 
         'green')

# Start the app, note that work will be saved every time the 
# label, filter, fill buttons are clicked within the app.
# Prior work saved in the label_dir will be loaded to resume labeling
p2t(umap_dir = umap_dir, 
    label_dir = lab_dir, 
    label_key = label_key, 
    label_col = pal)

# Now we'll make a basic random forest with our large amounts of training data,
# training it on all of our preprocessed data
train_dat <- do.call(rbind, lapply(
  FUN = function(x) {
    labels <- raster(file.path(lab_dir, x))
    predictors <- stack(file.path(preproc_dir, x))
    train_stack <- stack(labels, predictors)
    train_vals <- getValues(train_stack)
    pred_names <- c('label', paste0('X_', seq_len(nlayers(train_stack) - 1)))
    colnames(train_vals) <- pred_names
    return(as.data.frame(train_vals))
  },
  X = list.files(lab_dir)
))

set.seed(123)
rf_mod <- ranger(label ~ ., 
                 data = train_dat,
                 num.threads = umap_cores,
                 classification = TRUE)
print(rf_mod)

#Predict to some new data
og_ext <- extent(stack(image_dir))
mean_x <- mean(og_ext[1:2])
mean_y <- mean(og_ext[3:4])
pred_dir <- 'pred_dir'
dir.create(pred_dir)

tile_at_coords(coords = cbind(mean_x, mean_y - 25), 
               len_side = 25, 
               buffer = buff,
               out_dir = pred_dir,
               img = image_dir, 
               ncores = pre_cores)

pred_files <- list.files(pred_dir, full.names = TRUE)

pre_pipeline <- function(x, fs, b) {
  ndvi_msavi(x)
  sobel(x)
  mean_var(x, f_width = fs)
  remove_buffer(x, b)
}

mclapply(FUN = pre_pipeline,
         X = pred_files,
         mc.cores = pre_cores,
         fs = neighborhoods,
         b = buff)

pred_names <- colnames(train_dat[2:ncol(train_dat)])

new_dat <- do.call(rbind, mclapply(FUN = 
                                   function(x,p){setNames(as.data.frame(getValues(stack(x))), p)},
                                 X = pred_files,
                                 p = pred_names,
                                 mc.cores = pre_cores))

preds <- predict(rf_mod, new_dat, num.threads = umap_cores)

pred_ras <- raster(pred_files[1])
values(pred_ras) <- preds$predictions

leaflet() %>%
  addRasterRGB(stack(pred_files[1]),
               r = 1, g = 2, b = 3,
               group = 'RGB') %>%
  addRasterImage(pred_ras,
                 project = FALSE,
                 opacity = 1,
                 colors = c('transparent','red'),
                 group = 'Canopy') %>%
  addLayersControl(overlayGroups = c('Canopy'), 
                   options = layersControlOptions(collapsed = FALSE))

