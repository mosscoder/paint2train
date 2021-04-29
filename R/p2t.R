p2t <- function(umap_dir, label_dir, label_key, label_cols, map_height = 1000, ...) {
  umap_files <- list.files(umap_dir, full.names = TRUE)
  targ_crs <- raster::crs(raster::raster(umap_files[1]))
  band_count <- raster::nlayers(raster::stack(umap_files[1]))
  band_choices <- seq_len(band_count)
  lab_cols <- c('Black','Green','Blue','Red')
  
  split_format <- tags$head(tags$style(HTML(".shiny-split-layout > div { overflow: visible; }")))
  
  ui <- shiny::fluidPage(
    
    shiny::sidebarLayout(
    position = 'left',
    shiny::sidebarPanel(
      width = 3,
      shiny::selectInput(
        inputId = 'img_sel',
        label = 'Select image',
        choices = basename(umap_files)
      ),

      shiny::splitLayout(
        split_format,
        cellWidths = c('0%', '30%', '30%', '30%'),
        shiny::selectInput(
          inputId = 'b_1',
          label = 'R band',
          choices = band_choices,
          selected = 3,
          width = 100
        ),
        shiny::selectInput(
          inputId = 'b_2',
          label = 'G band',
          choices = band_choices,
          selected = 4,
          width = 100
        ),
        shiny::selectInput(
          inputId = 'b_3',
          label = 'B band',
          choices = band_choices,
          selected = 5,
          width = 100
        )
      ), 
      
      shiny::sliderInput(
        inputId = 'img_qt',
        label = 'Image quantiles',
        ticks = FALSE,
        value = c(0, 1),
        min = 0,
        max = 1
      ),
      
      hr(),
      
      shiny::sliderInput(
        inputId = 'thresh',
        label = 'Similarity threshold',
        ticks = FALSE,
        value = 0.1,
        min = 0,
        max = 1,
        step = 0.01
      ),
      shinyWidgets::switchInput(
        inputId = 'filter_noise',
        label = 'Filter\nnoise',
        value = TRUE,
        size = 'mini',
        onStatus = "success",
        offStatus = "danger"
      ), 
   
     
      shiny::selectInput(
        inputId = 'label_class',
        label = 'Label class',
        choices = names(label_key),
        selected = names(label_key)[0]
      ),
      shinyWidgets::actionBttn(inputId = 'assign',
                               label = 'Assign painted to class',
                               color = 'primary', 
                               style = 'material-flat',
                               size = 'sm'),
   
      hr(),
      
      shiny::sliderInput(
        inputId = 'paint_op',
        label = 'Paint opacity',
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
      )
      
    ),
    shiny::mainPanel(leaflet::leafletOutput('rgb_leaf', height = map_height), width = 9)
  ))
  
  server <- function(input, output) {
    
    fname <- shiny::reactive({
     file_ind <- which(basename(umap_files) == input$img_sel)
     umap_files[file_ind]
    })
    
    rgb_ras <- shiny::reactive({
      base_st <- raster::stack(fname())
      b1 <- base_st[[as.numeric(input$b_1)]]
      b2 <- base_st[[as.numeric(input$b_2)]]
      b3 <- base_st[[as.numeric(input$b_3)]]
      leaflet::projectRasterForLeaflet( raster::stack(b1,b2,b3), method = 'ngb')
    })
    
    umap_ras <- shiny::reactive({
      raster::stack(fname())[[1:2]]
    })
    
    umap_pts <- shiny::reactive({
      raster::rasterToPoints(umap_ras())[,3:4]
    })
    
    ras_qts <- shiny::reactive({
      c(input$img_qt[1], input$img_qt[2])
    })
    
    ras_bounds <- shiny::reactive({
      focal_e <- raster::projectExtent(umap_ras(), crs = raster::crs('+init=epsg:4326'))
      raster::extent(focal_e)
    })
    
    leaf_opts <- leaflet::leafletOptions(zoomControl = FALSE)
    
    output$rgb_leaf <- leaflet::renderLeaflet(leaflet::leaflet(options = leaf_opts) %>%
                                                leaflet::addMapPane("rgb_pane", zIndex = 410) %>%
                                                leaflet::addMapPane("class_pane", zIndex = 430) %>%
                                                leaflet::addMapPane("paint_pane", zIndex = 430) %>%
                                                leaflet::fitBounds(lng1 = ras_bounds()[1], 
                                                                   lng2 = ras_bounds()[2],
                                                                   lat1 = ras_bounds()[3],
                                                                   lat2 = ras_bounds()[4])
                                                ) 
    
    shiny::observe({
      leaflet::leafletProxy('rgb_leaf') %>%
        leaflet::clearGroup(group = 'rgb') %>%
        leafem::addRasterRGB(rgb_ras(), 
                             r = 1, 
                             g = 2,
                             b = 3,
                             quantiles = ras_qts(),
                             group = 'rgb',
                             maxBytes = 12 * 1024 * 1024
                              )
    })
    
    click_coords <- eventReactive(input$rgb_leaf_click, {
      click <- input$rgb_leaf_click
      if (is.null(click))
        return()
      
      click_xy <-
        sp::SpatialPoints(
          coords = data.frame(click$lng, click$lat),
          proj4string = raster::crs('+init=epsg:4326')
        )
      click_trans <- sp::spTransform(click_xy, targ_crs)
      
    })
    
    umap_vals <- eventReactive(input$rgb_leaf_click, {
      if (is.null(click_coords()))
        return()
      vals <- raster::extract(umap_ras(), click_coords())
      print(vals)
      vals
    })
    
    painted_ras <-
      eventReactive(c(input$rgb_leaf_click, input$thresh, input$filter_noise) , {
        if (is.null(umap_vals()[1]) | is.na(umap_vals()[1]))
          return()
        udf <- data.frame(u1 = raster::values(umap_ras()[[1]]),
                          u2 = raster::values(umap_ras())[[2]])
        
        dists <- RANN::nn2(data = umap_vals(),
                           query = umap_pts())$nn.dists %>% unlist()
        painted_ras <- umap_ras()[[1]]
        raster::values(painted_ras) <-
          ifelse(scales::rescale(dists) < input$thresh, 1, NA)
        
        if (isTRUE(input$filter_noise)) {
          f <-
            raster::focal(painted_ras,
                          FUN = sum,
                          na.rm = TRUE,
                          w = matrix(1, 3, 3))
          loners <- which(raster::values(f) == 1)
          raster::values(painted_ras)[loners] <- NA
        }
        
        painted_ras
        
      })
    
    observe({
      lab_file <- file.path(label_dir, basename(fname()))
      if(!file.exists(file.path(label_dir, basename(fname())))){
        label_ras <- umap_ras()[[1]]
        raster::values(label_ras) <- 0
        raster::writeRaster(label_ras, lab_file, overwrite = TRUE)
        }
    })
    
    observeEvent(input$assign,{
      lab_file <- file.path(label_dir, basename(fname()))
      label_ras <- raster::raster(lab_file)
    
      pix_to_class <- which(raster::values(painted_ras()) == 1)
      class_val <- label_key[which(names(label_key) == input$label_class)] %>% unlist()
      
      raster::values(label_ras)[pix_to_class] <- class_val
      
      if (isTRUE(input$filter_noise)) {
        f <-
          raster::focal(label_ras,
                        FUN = sum,
                        na.rm = TRUE,
                        w = matrix(1, 3, 3))
        loners <- which(raster::values(f) == 1)
        raster::values(label_ras)[loners] <- NA
      }
      
      raster::writeRaster(label_ras, lab_file, overwrite = TRUE)
    })
    
    shiny::observeEvent(
      c(
        input$img_sel,
        input$rgb_leaf_click,
        input$paint_col,
        input$paint_op,
        input$filter_noise,
        input$thresh,
        input$img_qt,
        input$b_1,
        input$b_2,
        input$b_3,
        input$assign
      ),
      {
     
        if (is.null(umap_vals()[1]) | is.na(umap_vals()[1]) )
          return()
        
        labs <- raster::raster(file.path(label_dir, basename(fname())))
        
        leaflet::leafletProxy(map = 'rgb_leaf') %>%
          leaflet::clearControls() %>%
          leaflet::clearGroup(group = 'Currently painted') %>%
          leaflet::clearGroup(group = 'Labeled') %>%
          leaflet::addRasterImage(
            labs,
            colors = label_cols,
            project = TRUE,
            group = 'Labeled',
            method = 'ngb'
          ) %>%
          leaflet::addRasterImage(
            painted_ras(),
            color = tolower(input$paint_col),
            project = TRUE,
            opacity = input$paint_op,
            group = 'Currently painted',
            method = 'ngb'
          ) %>%
          leaflet::addLayersControl(overlayGroups = c('Currently painted', 'Labeled'),
                                    options = leaflet::layersControlOptions(collapsed = FALSE)) 
        
      }
    )
    
    
  }
  
  shiny::shinyApp(ui = ui, server = server)
  
}
