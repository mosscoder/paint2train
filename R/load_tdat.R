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