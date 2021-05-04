library(raster)
library(leaflet)
library(leafem)
library(htmlwidgets)
library(shinyWidgets)
library(tidyverse)
library(RANN)
library(sp)
library(rgdal)


label_key <- list(Unknown = 0,
                  `Not woody` = 1,
                  `Woody` = 2)
label_cols <- c('royalblue', 'tan', 'green')

umap_dir <- 'www/umap_tiles'
label_dir <- 'www/label_tiles'

map_height = 1000
r_band = 4
g_band = 5
b_band = 6
nir_band = 7

umap_files <- list.files(umap_dir, full.names = TRUE)
targ_crs <- raster::crs(raster::raster(umap_files[1]))
band_count <- raster::nlayers(raster::stack(umap_files[1]))
band_choices <- seq_len(band_count)

pf <- raster::raster(umap_files[1])[[1]]
values(pf) <- NA
paint_file <- file.path(label_dir, 'paint_in_prog.tif')
raster::writeRaster(pf, paint_file, overwrite = TRUE)

split_format <-
  shiny::tags$head(shiny::tags$style(
    htmltools::HTML(".shiny-split-layout > div { overflow: visible; }")
  ))

ui <- shiny::fluidPage(shiny::sidebarLayout(
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
      label = 'Map 1 quantiles',
      ticks = FALSE,
      value = c(0.02, 0.98),
      min = 0,
      max = 1
    ),
    
    shiny::hr(),
    
    shiny::sliderInput(
      inputId = 'thresh',
      label = 'Similarity threshold',
      ticks = FALSE,
      value = 0.1,
      min = 0,
      max = 1,
      step = 0.005
    ),
    shinyWidgets::actionBttn(
      inputId = 'filter_noise',
      label = 'Filter\nnoise',
      color = 'success',
      style = 'material-flat',
      size = 'sm'
      
    ),
    
    shiny::hr(),
    
    shiny::selectInput(
      inputId = 'label_class',
      label = 'Label class',
      choices = names(label_key),
      selected = names(label_key)[0]
    ),
    
    shinyWidgets::actionBttn(
      inputId = 'assign',
      label = 'Assign painted to class',
      color = 'primary',
      style = 'material-flat',
      size = 'sm'
    ),
    shiny::hr(),
    
    shinyWidgets::actionBttn(
      inputId = 'fill_remainder',
      label = 'Fill unlabeled as class',
      color = 'danger',
      style = 'material-flat',
      size = 'sm'
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
    
    shiny::radioButtons(
      inputId = 'paint_col',
      label = 'Paint color',
      choices = c('Red', 'Green', 'Blue', 'Cyan'),
      inline = TRUE
    )
    
  ),
  shiny::mainPanel(width = 9,
                   leaflet::leafletOutput('m1_leaf', height = map_height))
))

server <- function(input, output, session) {
  fname <- shiny::reactive({
    file_ind <- which(basename(umap_files) == input$img_sel)
    umap_files[file_ind]
  })
  
  band_inds <-
    shiny::reactive({
      as.numeric(c(1:3, input$b_1, input$b_2, input$b_3, input$b_4))
    })
  
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
    raster::rasterToPoints(umap_ras())[, 3:5]
  })
  
  ras_bounds <- shiny::reactive({
    focal_e <-
      raster::projectExtent(umap_ras(), crs = raster::crs('+init=epsg:4326'))
    raster::extent(focal_e)
  })
  
  leaf_opts <- leaflet::leafletOptions(zoomControl = FALSE)
  
  
  output$m1_leaf <-
    leaflet::renderLeaflet(
      leaflet::leaflet(options = leaf_opts) %>%
        leaflet::fitBounds(
          lng1 = ras_bounds()[1],
          lng2 = ras_bounds()[2],
          lat1 = ras_bounds()[3],
          lat2 = ras_bounds()[4]
        )
    )
  
  
  shiny::observe({
    qlims <- c(input$img_qt_1[1], input$img_qt_1[2])
    
    leaflet::leafletProxy('m1_leaf') %>%
      clearGroup('Trule color') %>%
      clearGroup('UMAP') %>%
      clearGroup('NIR false color') %>%
      leafem::addRasterRGB(
        base_ras()[[4:6]],
        r = 1,
        g = 2,
        b = 3,
        quantiles = qlims,
        group = 'True color',
        maxBytes = 12 * 1024 * 1024,
        project = FALSE
      ) %>%
      leafem::addRasterRGB(
        base_ras()[[1:3]],
        r = as.numeric(input$u_1),
        g = as.numeric(input$u_2),
        b = as.numeric(input$u_3),
        quantiles = qlims,
        group = 'UMAP',
        maxBytes = 12 * 1024 * 1024,
        project = FALSE
      ) %>%
      leafem::addRasterRGB(
        base_ras()[[c(7, 5, 6)]],
        r = 1,
        g = 2,
        b = 3,
        quantiles = qlims,
        group = 'NIR false color',
        maxBytes = 12 * 1024 * 1024,
        project = FALSE
      ) %>%
      leaflet::addLayersControl(
        overlayGroups = c(
          'True color',
          'UMAP',
          'NIR false color',
          'Currently painted',
          'Labeled'
        ),
        options = leaflet::layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup('UMAP') %>%
      hideGroup('NIR false color')
  })
  
  
  click_coords <- eventReactive(input$m1_leaf_click, {
    click <- input$m1_leaf_click
    if (is.null(click))
      return()
    
    click_xy <-
      sp::SpatialPoints(
        coords = data.frame(click$lng, click$lat),
        proj4string = raster::crs('+init=epsg:4326')
      )
    click_trans <- sp::spTransform(click_xy, targ_crs)
    
  })
  
  umap_vals <- eventReactive(input$m1_leaf_click, {
    if (is.null(click_coords()))
      return()
    vals <- raster::extract(umap_ras(), click_coords())
    print(vals)
    vals
  })
  
  observe({
    lab_file <- file.path(label_dir, basename(fname()))
    if (!file.exists(file.path(label_dir, basename(fname())))) {
      label_ras <- umap_ras()[[1]]
      raster::values(label_ras) <- NA
      raster::writeRaster(label_ras, lab_file, overwrite = TRUE)
    }
  })
  
  observeEvent(c(input$m1_leaf_click, input$thresh) , {
    if (is.null(umap_vals()[1]) | is.na(umap_vals()[1]))
      return()
    udf <- data.frame(
      u1 = raster::values(umap_ras()[[1]]),
      u2 = raster::values(umap_ras()[[2]]),
      u3 = raster::values(umap_ras()[[3]])
    )
    
    dists <- RANN::nn2(data = umap_vals(),
                       query = umap_pts())$nn.dists %>% unlist()
    painted_ras <- umap_ras()[[1]]
    raster::values(painted_ras) <-
      ifelse(scales::rescale(dists) < input$thresh, 1, NA)
    
    raster::writeRaster(painted_ras, paint_file, overwrite = TRUE)
    
  })
  
  observeEvent(input$filter_noise, {
    painted_ras <-
      raster::raster(paint_file)
    
    f <-
      raster::focal(painted_ras,
                    FUN = sum,
                    na.rm = TRUE,
                    w = matrix(1, 3, 3))
    loners <- which(raster::values(f) == 1)
    raster::values(painted_ras)[loners] <- NA
    
    raster::writeRaster(painted_ras, paint_file, overwrite = TRUE)
  })
  
  observeEvent(input$assign, {
    lab_file <- file.path(label_dir, basename(fname()))
    label_ras <- raster::raster(lab_file)
    painted_ras <- raster::raster(paint_file)
    
    pix_to_class <- which(raster::values(painted_ras) == 1)
    class_val <-
      label_key[which(names(label_key) == input$label_class)] %>% unlist()
    
    raster::values(label_ras)[pix_to_class] <- class_val
    
    raster::writeRaster(label_ras, lab_file, overwrite = TRUE)
  })
  
  observeEvent(input$fill_remainder, {
    lab_file <- file.path(label_dir, basename(fname()))
    label_ras <- raster::raster(lab_file)
    
    pix_to_class <- which(raster::values(is.na(label_ras)))
    class_val <-
      label_key[which(names(label_key) == input$label_class)] %>% unlist()
    
    raster::values(label_ras)[pix_to_class] <- class_val
    
    raster::writeRaster(label_ras, lab_file, overwrite = TRUE)
  })
  
  shiny::observeEvent(
    c(
      input$m1_leaf_click,
      input$paint_col,
      input$paint_op,
      input$filter_noise,
      input$fill_remainder,
      input$thresh,
      input$assign,
      input$m1
    ),
    {
      if (is.null(umap_vals()[1]) | is.na(umap_vals()[1]))
        return()
      
      labs <-
        raster::raster(file.path(label_dir, basename(fname())))
      painted_ras <- raster::raster(paint_file)
      
      class_pal <-
        leaflet::colorNumeric(palette = label_cols,
                              domain = label_key,
                              na.color = 'transparent')
      
      leaflet::leafletProxy(map = 'm1_leaf') %>%
        leaflet::clearControls() %>%
        leaflet::clearGroup(group = 'Currently painted') %>%
        leaflet::clearGroup(group = 'Labeled') %>%
        leaflet::clearGroup(group = 'legend') %>%
        leaflet::addRasterImage(
          labs,
          colors = class_pal,
          project = TRUE,
          group = 'Labeled',
          method = 'ngb'
        ) %>%
        leaflet::addRasterImage(
          painted_ras,
          color = input$paint_col,
          project = TRUE,
          opacity = input$paint_op,
          group = 'Currently painted',
          method = 'ngb'
        ) %>%
        leaflet::addLayersControl(
          overlayGroups = c(
            'True color',
            'UMAP',
            'NIR false color',
            'Currently painted',
            'Labeled'
          ),
          options = leaflet::layersControlOptions(collapsed = FALSE)
        ) %>%
        leaflet::addLegend(
          position = 'bottomright',
          colors = label_cols,
          labels = names(label_key),
          title = 'Classes',
          group = 'legend'
        )
      
      
      
    }
  )
  
  session$onSessionEnded(function() {
    if (file.exists(paint_file))
      file.remove(paint_file)
  })
  
}

shiny::shinyApp(ui = ui, server = server)
