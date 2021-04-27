p2t <- function(umap_dir, label_dir, ...) {
  umap_files <- list.files(umap_dir, full.names = TRUE)
  targ_crs <- raster::crs(raster::raster(umap_files[1]))
  band_count <- raster::nlayers(raster::stack(umap_files[1]))
  band_choices <- seq_len(band_count)
  
  ui <- shiny::fillPage(
    leaflet::leafletOutput('rgb_leaf', height = '90%', width = '100%'),
    shiny::absolutePanel(
      shiny::selectInput(
        inputId = 'img_sel',
        label = 'Select image',
        choices = basename(umap_files)
      ),
      top = 10,
      left = 10
    ),
    shiny::absolutePanel(
      shiny::sliderInput(inputId = 'img_qt',
                         label = 'Image quantiles',
                         ticks = FALSE,
                         value = c(0.01,0.99),
                         min = 0,
                         max = 1,
                         step = 0.01),
      top = 75,
      left = 10,
      width = 150
    ), 
    
    shiny::absolutePanel(shiny::selectInput(
      inputId = 'b_1',
      label = 'R band',
      choices = band_choices,
      selected = 3
    ),
    shiny::selectInput(
      inputId = 'b_2',
      label = 'G band',
      choices = band_choices,
      selected = 4
    ),
    shiny::selectInput(
      inputId = 'b_3',
      label = 'B band',
      choices = band_choices,
      selected = 5
    ),
    top = 155,
    left = 10,
    width = 60),
    
    shiny::absolutePanel(
      bottom = 10,
      left = 10,
      width = 200,
      shiny::sliderInput(
        inputId = 'thresh',
        label = 'Similarity threshold',
        ticks = FALSE,
        value = 0.1,
        min = 0,
        max = 1,
        step = 0.01
      )
    ),
    
    shiny::absolutePanel(
      bottom = 8,
      left = 250,
      width = 150,
      shiny::radioButtons(
        inputId = 'cand_col',
        label = 'Candidate color',
        choices = c('Red','Green')
      )
    ),
    
    shiny::absolutePanel(
      bottom = 30,
      left = 375,
      width = 150,
      shinyWidgets::switchInput(
        inputId = 'filter_noise',
        label = 'Filter noise',
        value = TRUE,
        size = 'mini',
        onStatus = "success",
        offStatus = "danger"
      )
    )
    
    
  )
  
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
      # list(lng = mean(focal_e[1],focal_e[2]),
      #      lat = mean(focal_e[3],focal_e[4]))
    })
    
    leaf_opts <- leaflet::leafletOptions(zoomControl = FALSE)
    
    output$rgb_leaf <- leaflet::renderLeaflet(leaflet::leaflet(options = leaf_opts) %>%
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
                             group = 'rgb')
    })
    
    click_coords <- eventReactive(input$rgb_leaf_click, {
      click <- input$rgb_leaf_click
      print(input$rgb_leaf_click)
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
      raster::extract(umap_ras(), click_coords())
    })
    
    painted_ras <- eventReactive(c(input$rgb_leaf_click, input$thresh, input$filter_noise) , {
      if (is.null(umap_vals()))
        return()
      udf <- data.frame(u1 = raster::values(umap_ras()[[1]]),
                        u2 = raster::values(umap_ras())[[2]])
      
      dists <- RANN::nn2(data = umap_vals(),
                         query = umap_pts())$nn.dists %>% unlist()
      painted_ras <- umap_ras()[[1]]
      raster::values(painted_ras) <- ifelse(scales::rescale(dists) < input$thresh, 1, NA)
      
      if(isTRUE(input$filter_noise)) {
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
    
    # painted_ras <- eventReactive(input$filter_noise, {
    #   r <- painted_ras()
    #   f <- raster::focal(r, FUN = sum, na.rm = TRUE, w=matrix(1,3,3))
    #   loners <- which(raster::values(f) == 1)
    #   raster::values(r)[loners] <- NA
    #   r
    # })
    
    shiny::observe({
      leaflet::leafletProxy(map = 'rgb_leaf') %>%
        leaflet::clearControls() %>%
        leaflet::clearGroup(group = 'Candidate') %>%
        leaflet::addRasterImage(painted_ras(),
                                color = tolower(input$cand_col),
                                project = TRUE,
                                opacity = 1,
                                group = 'Candidate',
                                method = 'ngb') %>%
        leaflet::addLayersControl(overlayGroups = 'Candidate')
      
    })
    
    
  }
  
  shiny::shinyApp(ui = ui, server = server)
  
}