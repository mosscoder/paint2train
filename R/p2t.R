#' @title Run Shiny app to label spatial data and imagery
#' @description Provide pt2 with a directory containing UMAP tile data, a directory
#' to save labeled tiles, metadata about target classes, and band indices 
#' of imagery. The function then launches a Shiny app in which users
#' may label imagery by a variety of mechanisms. These label data are
#' saved as .tif files in which integer values correspond to 
#' those provided in the label_key. 
#' 
#' @param umap_dir Path to UMAP tiles. 
#' @param label_dir Path where label data .tifs will be saved. 
#' @param label_key A named list of integers corresponding to target label classes.
#' @param label_cols Color palette for representing labeled classes. 
#' @param r_band The index of the red wavelength band. Because the umap_tile function appends 
#' input imagery with the 3 UMAP axes at the beginning of the raster stack, add 3 to the index 
#' of the band before pre-processing. 
#' @param g_band The index of the green wavelength band. Because the umap_tile function appends 
#' input imagery with the 3 UMAP axes at the beginning of the raster stack, add 3 to the index 
#' of the band before pre-processing. 
#' @param b_band The index of the blue wavelength band. Because the umap_tile function appends 
#' input imagery with the 3 UMAP axes at the beginning of the raster stack, add 3 to the index 
#' of the band before pre-processing. 
#' @param nir_band The index of the near-infrared wavelength band. Because the umap_tile function appends 
#' input imagery with the 3 UMAP axes at the beginning of the raster stack, add 3 to the index 
#' of the band before pre-processing. 
#' 
#' @return None. Tiles are saved to label_dir. 
#' 
#' @examples 
#' library(paint2train)
#' 
#' image_dir <- tempfile()
#' image_url <- 'https://github.com/mosscoder/paint2train/blob/main/data/sample_4band.tif?raw=true'
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
#'                    detectCores() - 1,
#'                    1) 
#' umap_cores <- detectCores() - 1                  
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
#' pal <- list('royalblue',
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
#' @export
p2t <- function(umap_dir, label_dir, label_key, label_cols,
                r_band = 4, g_band = 5, b_band = 6, nir_band = 7) {
  umap_files <- list.files(umap_dir, full.names = TRUE)
  targ_crs <- raster::crs(raster::raster(umap_files[1]))
  band_count <- raster::nlayers(raster::stack(umap_files[1]))
  band_choices <- seq_len(band_count)
  
  paint_files <- file.path(tempdir(), 
                           stringr::str_replace(basename(umap_files), 
                                       pattern = '.tif', '_paint.tif')
  )
  
  for(i in seq_along(paint_files)){
    pf <- raster::raster(umap_files[i])[[1]]
    raster::values(pf) <- NA
    raster::writeRaster(pf, paint_files[i], overwrite = TRUE)
  }
  
  
  split_format <- shiny::tags$head(shiny::tags$style(htmltools::HTML(".shiny-split-layout > div { overflow: visible; }")))
  abs_style <- "background:white; padding: 20px 20px 20px 20px; border-radius: 5pt;"
  
  ui <- shiny::fillPage(
    shiny::tags$style('.leaflet-container { cursor: auto !important; }' ),
    
    leaflet::leafletOutput('leafmap', height = '100vh'),
    shinybusy::use_busy_spinner(spin = "fingerprint", position = "bottom-left",  color = '#ff1d5e'),
    
    shiny::absolutePanel(
      top = 10,
      left = 10,
      width = 300,
      style = abs_style,
      draggable = TRUE,
      shiny::selectInput(
        inputId = 'img_sel',
        label = 'Select image',
        choices = basename(umap_files)
      ),
      htmltools::HTML('<button data-toggle="collapse" data-target="#demo">Aesthetics controls</button>'),
      shiny::tags$div(id = 'demo',  class="collapse",
                      shiny::splitLayout(
                        split_format,
                        cellWidths = c('0%', '25%', '25%', '25%', '25%'),
                        shiny::selectInput(
                          inputId = 'b_1',
                          label = 'R band',
                          choices = band_choices,
                          selected = 4,
                          width = 100
                        ),
                        shiny::selectInput(
                          inputId = 'b_2',
                          label = 'G band',
                          choices = band_choices,
                          selected = 5,
                          width = 100
                        ),
                        shiny::selectInput(
                          inputId = 'b_3',
                          label = 'B band',
                          choices = band_choices,
                          selected = 6,
                          width = 100
                        ),
                        shiny::selectInput(
                          inputId = 'b_4',
                          label = 'NIR band',
                          choices = band_choices,
                          selected = 7,
                          width = 100
                        )
                      ),
                      
                      shiny::splitLayout(
                        split_format,
                        cellWidths = c('0%', '33%', '33%', '33%'),
                        shiny::selectInput(
                          inputId = 'u_1',
                          label = 'UMAP R',
                          choices = 1:3,
                          selected = 1,
                          width = 100
                        ),
                        shiny::selectInput(
                          inputId = 'u_2',
                          label = 'UMAP G',
                          choices = 1:3,
                          selected = 2,
                          width = 100
                        ),
                        shiny::selectInput(
                          inputId = 'u_3',
                          label = 'UMAP B',
                          choices = 1:3,
                          selected = 3,
                          width = 100
                        )
                      ),
                      
                      shiny::sliderInput(
                        inputId = 'img_qt_1',
                        label = 'Baselayer quantiles',
                        ticks = FALSE,
                        value = c(0.02, 0.98),
                        min = 0,
                        max = 1
                      ),
                      
                      shiny::hr(),
                      
                      shiny::sliderInput(
                        inputId = 'paint_op',
                        label = 'Paint opacity',
                        ticks = FALSE,
                        value = 1,
                        min = 0,
                        max = 1,
                        step = 0.01
                      ),
                      
                      shiny::hr(),
                      
                      shiny::sliderInput(
                        inputId = 'lab_op',
                        label = 'Label opacity',
                        ticks = FALSE,
                        value = 1,
                        min = 0,
                        max = 1,
                        step = 0.01
                      ),
                      
                      shiny::radioButtons(
                        inputId = 'paint_col',
                        label = 'Paint color',
                        choices = c('Red', 'Green', 'Blue', 'Cyan'),
                        inline = TRUE
                      ))
    ),
    
    shiny::absolutePanel(top = 260,
                         right = 15,
                         draggable = TRUE,
                         width = 250,
                         style = abs_style,
                         shiny::h4("Labeling Tools"),
                         
                         shiny::selectInput(
                           inputId = 'label_class',
                           label = 'Select class to label:',
                           choices = names(label_key),
                           selected = names(label_key)[0]
                         ),
                         shiny::sliderInput(
                           inputId = 'click_sens',
                           label = 'Initial sensitivity',
                           ticks = FALSE,
                           value = 20,
                           min = 2,
                           max = 100,
                           step = 1
                         ),
                         shiny::sliderInput(
                           inputId = 'thresh',
                           label = 'Dissimilarity threshold',
                           ticks = FALSE,
                           value = 0.1,
                           min = 0,
                           max = 1,
                           step = 0.005
                         ),
                         
                         shiny::actionButton(inputId = 'assign',
                                             label = 'Label painted areas',
                                             class = 'btn-primary'),
                         
                         shiny::hr(),
                         shiny::actionButton(inputId = 'assign_draw',
                                             label = 'Label drawn areas',
                                             class = 'btn-warning'),
                         
                         shiny::hr(),
                         shiny::actionButton(inputId = 'filter_noise',
                                             label = 'Filter lone pixels',
                                             class = 'btn-success'),
                         shiny::hr(),
                         shiny::actionButton(inputId = 'fill_remainder',
                                             label = 'Fill unlabeled as class',
                                             class = 'btn-danger')
    )
  )
  
  
  
  server <- function(input, output, session) {
    
    fname <- shiny::reactive({
      file_ind <- which(basename(umap_files) == input$img_sel)
      umap_files[file_ind]
    })
    
    paint_file <- shiny::reactive({
      file_ind <- which(basename(umap_files) == input$img_sel)
      paint_files[file_ind]
    })
    
    band_inds <- shiny::reactive({as.numeric(c(1:3,input$b_1, input$b_2, input$b_3, input$b_4))})
    
    init_proj <- shiny::reactive({
      leaflet::projectRasterForLeaflet(raster::stack(fname())[[band_inds()]], method = 'bilinear')
    })
    
    base_ras <- shiny::reactive({
      sub_inds <- list(umap = 1:3,
                       true = band_inds()[4:6],
                       false = band_inds()[c(7, 5, 6)])
      
      raster::stack(init_proj()[[unlist(sub_inds)]])
    })
    
    umap_ras <- shiny::reactive({
      raster::stack(fname())[[1:3]]
    })
    
    umap_pts <- shiny::reactive({
      raster::rasterToPoints(umap_ras())[,3:5]
    })
    
    ras_bounds <- shiny::reactive({
      focal_e <- raster::projectExtent(umap_ras(), crs = raster::crs('+init=epsg:4326'))
      raster::extent(focal_e)
    })
    
    leaf_opts <- leaflet::leafletOptions(zoomControl = FALSE)
    
    
    output$leafmap <-
      leaflet::renderLeaflet(
        
        leaflet::leaflet(options = leaf_opts) %>%
          leaflet::fitBounds(
            lng1 = ras_bounds()[1],
            lng2 = ras_bounds()[2],
            lat1 = ras_bounds()[3],
            lat2 = ras_bounds()[4]
          ) %>%
          leaflet::addLegend(
            position = 'topright',
            colors = label_cols,
            labels = names(label_key),
            title = 'Classes',
            group = 'legend'
          ) %>%
          leaflet::addLayersControl(
            baseGroups = c('True color', 'UMAP', 'NIR false color'),
            overlayGroups = c('Currently painted', 'Classes Labeled'),
            options = leaflet::layersControlOptions(collapsed = FALSE,
                                                    autoZIndex = TRUE),
            position = 'bottomright'
          ) %>%
          leaflet.extras::addDrawToolbar(position = 'bottomright', 
                                         singleFeature = TRUE,
                                         polylineOptions = FALSE,
                                         markerOptions = FALSE,
                                         circleMarkerOptions = FALSE,
                                         circleOptions = FALSE,
                                         editOptions = leaflet.extras::editToolbarOptions(edit = FALSE, remove = TRUE)) 
      ) 
    
    
    shiny::observe({
      shinybusy::show_spinner()
      leaflet::leafletProxy('leafmap') %>%
        leaflet::clearControls() %>%
        leaflet::clearGroup('Trule color') %>%
        leaflet::clearGroup('UMAP') %>%
        leaflet::clearGroup('NIR false color') %>%
        leafem::addRasterRGB(base_ras()[[c(7,5,6)]], 
                             r = 1, 
                             g = 2,
                             b = 3,
                             quantiles = c(input$img_qt_1[1],input$img_qt_1[2]),
                             group = 'NIR false color',
                             maxBytes = 12 * 1024 * 1024, 
                             project = FALSE
        ) %>%
        leafem::addRasterRGB(base_ras()[[1:3]], 
                             r = 1, 
                             g = 2,
                             b = 3,
                             quantiles = c(input$img_qt_1[1],input$img_qt_1[2]),
                             group = 'UMAP',
                             maxBytes = 12 * 1024 * 1024, 
                             project = FALSE
        ) %>%
        leafem::addRasterRGB(base_ras()[[4:6]], 
                             r = 1, 
                             g = 2,
                             b = 3,
                             quantiles = c(input$img_qt_1[1],input$img_qt_1[2]),
                             group = 'True color',
                             maxBytes = 12 * 1024 * 1024, 
                             project = FALSE
        ) %>%
        leaflet::addLegend(position = 'topright',
                           colors = label_cols,
                           labels = names(label_key),
                           title = 'Classes', 
                           group = 'legend') %>%
        leaflet::addLayersControl(baseGroups = c('True color', 'UMAP', 'NIR false color'),
                                  overlayGroups = c('Currently painted', 'Classes Labeled'),
                                  options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
        leaflet::hideGroup('UMAP') %>%
        leaflet::hideGroup('NIR false color') 
      shinybusy::hide_spinner()
    })
    
    shiny::observeEvent(c(input$u_1, input$u_2, input$u_3), {
      
      leaflet::leafletProxy('leafmap') %>%
        leaflet::clearGroup('UMAP') %>%
        leafem::addRasterRGB(base_ras()[[1:3]], 
                             r = as.numeric(input$u_1), 
                             g = as.numeric(input$u_2),
                             b = as.numeric(input$u_3),
                             quantiles = c(input$img_qt_1[1],input$img_qt_1[2]),
                             group = 'UMAP',
                             maxBytes = 12 * 1024 * 1024, 
                             project = FALSE
        ) 
      
    })
    
    click_coords <- shiny::eventReactive(input$leafmap_click, {
      click <- input$leafmap_click
      if (is.null(click))
        return()
      
      click_xy <-
        sp::SpatialPoints(
          coords = data.frame(click$lng, click$lat),
          proj4string = raster::crs('+init=epsg:4326')
        )
      click_trans <- sp::spTransform(click_xy, targ_crs)
      
    })
    
    edit_status <- shiny::reactiveValues(status = FALSE)
    
    shiny::observeEvent(input$leafmap_draw_start, {
      edit_status$status <- TRUE
    })
    
    shiny::observeEvent(input$leafmap_draw_stop, {
      edit_status$status <- FALSE
    })
    
    dist_vals <- shiny::eventReactive(input$leafmap_click, {
      if (is.null(click_coords()))
        return()
      shinybusy::show_spinner()
      vals <- raster::extract(umap_ras(), click_coords()) %>% as.data.frame()
      
      if (is.na(vals[1]))
        return()
      
      colnames(vals) <- c('u1','u2','u3')
      
      udf <- data.frame(u1 = raster::values(umap_ras()[[1]]),
                        u2 = raster::values(umap_ras()[[2]]),
                        u3 = raster::values(umap_ras()[[3]]))
      
      dist <- RANN::nn2(data = vals,
                        query = umap_pts())$nn.dists %>% unlist() %>% scales::rescale()
      
      clst <- kmeans(rbind(vals, udf), centers = input$click_sens)$cluster
      inclust <- which(clst[-1] == clst[1])
      
      painted_ras <- raster::raster(paint_file())
      raster::values(painted_ras) <- NA
      raster::values(painted_ras)[inclust] <- 1
      raster::writeRaster(painted_ras, paint_file(), overwrite = TRUE)
      
      shiny::updateSliderInput(inputId = "thresh", value = max(dist[inclust]))
      shinybusy::hide_spinner()
      dist
    })
    
    shiny::observe({
      lab_file <- file.path(label_dir, basename(fname()))
      if(!file.exists(lab_file)){
        label_ras <- umap_ras()[[1]]
        raster::values(label_ras) <- NA
        raster::writeRaster(label_ras, lab_file, overwrite = TRUE)
      }
    })
    
    
    shiny::observeEvent(input$thresh , {
      if (is.na(dist_vals()[1]) | isTRUE(edit_status$status))
        return()
      
      painted_ras <- raster::raster(paint_file())
      raster::values(painted_ras) <-
        ifelse(dist_vals() < input$thresh, 1, NA)
      
      raster::writeRaster(painted_ras, paint_file(), overwrite = TRUE)
      
    })
    
    shiny::observeEvent(input$filter_noise, {
      painted_ras <-
        raster::raster(paint_file())
      
      f <-
        raster::focal(painted_ras,
                      FUN = sum,
                      na.rm = TRUE,
                      w = matrix(1, 3, 3))
      loners <- which(raster::values(f) == 1)
      raster::values(painted_ras)[loners] <- NA
      
      raster::writeRaster(painted_ras, paint_file(), overwrite = TRUE)
    })
    
    shiny::observeEvent(input$assign,{
      lab_file <- file.path(label_dir, basename(fname()))
      label_ras <- raster::raster(lab_file)
      painted_ras <- raster::raster(paint_file())
      pix_to_class <- which(raster::values(painted_ras) == 1)
      class_val <- label_key[which(names(label_key) == input$label_class)] %>% unlist()
      raster::values(label_ras)[pix_to_class] <- class_val
      raster::writeRaster(label_ras, lab_file, overwrite = TRUE)
    })
    
    shiny::observeEvent(input$assign_draw,{
      lab_file <- file.path(label_dir, basename(fname()))
      label_ras <- raster::raster(lab_file)
      polygon_coordinates <- input$leafmap_draw_new_feature$geometry$coordinates[[1]]
      drawn_polygon <- do.call(rbind,lapply(polygon_coordinates,function(x){c(x[[1]][1],x[[2]][1])}))
      drawn_polygon <- sp::Polygons(list(sp::Polygon(drawn_polygon)), ID = 1)
      drawn_polygon <- sp::SpatialPolygons(list(drawn_polygon))
      sp::proj4string(drawn_polygon) <- '+init=epsg:4326'
      drawn_polygon <- sp::spTransform(drawn_polygon, targ_crs)
      pix_to_class <- raster::cellFromPolygon(label_ras, drawn_polygon) %>% unlist()
      class_val <- label_key[which(names(label_key) == input$label_class)] %>% unlist()
      raster::values(label_ras)[pix_to_class] <- class_val
      raster::writeRaster(label_ras, lab_file, overwrite = TRUE)
    })
    
    shiny::observeEvent(input$fill_remainder,{
      lab_file <- file.path(label_dir, basename(fname()))
      label_ras <- raster::raster(lab_file)
      
      pix_to_class <- which(raster::values(is.na(label_ras)))
      class_val <- label_key[which(names(label_key) == input$label_class)] %>% unlist()
      
      raster::values(label_ras)[pix_to_class] <- class_val
      
      raster::writeRaster(label_ras, lab_file, overwrite = TRUE)
    })
    
    paint_r <- shiny::reactive({raster::raster(paint_file())})
    lab_r <- shiny::reactive({raster::raster(file.path(label_dir, basename(fname())))})
    
    shiny::observeEvent(
      c(
        input$leafmap_click,
        input$paint_col,
        input$paint_op,
        input$thresh
      ),{
        
        if(isTRUE(edit_status$status))
          return()
        
        shiny::req(dist_vals())
        painted_ras <- shiny::req(paint_r())
        
        leaflet::leafletProxy(map = 'leafmap') %>%
          leaflet::clearControls() %>%
          leaflet::clearGroup(group = 'Currently painted') %>%
          leaflet::clearGroup(group = 'legend') %>%
          leaflet::addRasterImage(
            painted_ras,
            color = input$paint_col,
            project = TRUE,
            opacity = input$paint_op,
            group = 'Currently painted',
            method = 'ngb'
          ) %>%
          leaflet::addLegend(position = 'topright',
                             colors = label_cols,
                             labels = names(label_key),
                             title = 'Classes', 
                             group = 'legend') %>%
          leaflet::addLayersControl(baseGroups = c('True color', 'UMAP', 'NIR false color'),
                                    overlayGroups = c('Currently painted', 'Classes Labeled'),
                                    options = leaflet::layersControlOptions(collapsed = FALSE)) 
        
        
        
      }
    )
    
    shiny::observeEvent(
      c(input$img_sel,
        input$lab_op,
        input$filter_noise,
        input$fill_remainder,
        input$assign,
        input$assign_draw
      ),{
        
        if(isTRUE(edit_status$status))
          return()
        
        labs <- shiny::req(lab_r())
        
        class_pal <- leaflet::colorNumeric(palette = label_cols, domain = label_key, na.color = 'transparent')
        
        leaflet::leafletProxy(map = 'leafmap') %>%
          leaflet::clearControls() %>%
          leaflet::clearGroup(group = 'Classes Labeled') %>%
          leaflet::clearGroup(group = 'legend') %>%
          leaflet::addRasterImage(
            labs,
            colors = class_pal,
            opacity = input$lab_op,
            project = TRUE,
            group = 'Classes Labeled',
            method = 'ngb'
          ) %>%
          leaflet::addLegend(position = 'topright',
                             colors = label_cols,
                             labels = names(label_key),
                             title = 'Classes', 
                             group = 'legend') %>%
          leaflet::addLayersControl(baseGroups = c('True color', 'UMAP', 'NIR false color'),
                                    overlayGroups = c('Currently painted', 'Classes Labeled'),
                                    options = leaflet::layersControlOptions(collapsed = FALSE)) 
        
        
        
      }
    )
    
    session$onSessionEnded(function() {
      file.remove(paint_files)
    })
    
  }
  
  shiny::shinyApp(ui = ui, server = server)
  
}



