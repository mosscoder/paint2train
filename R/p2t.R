#' @export
p2t <- function(umap_dir, label_dir, label_key, label_cols,
                r_band = 4, g_band = 5, b_band = 6, nir_band = 7, ...) {
  umap_files <- list.files(umap_dir, full.names = TRUE)
  targ_crs <- raster::crs(raster::raster(umap_files[1]))
  band_count <- raster::nlayers(raster::stack(umap_files[1]))
  band_choices <- seq_len(band_count)
  lab_cols <- c('Black','Green','Blue','Red')
  
  pf <- raster::raster(umap_files[1])[[1]]
  raster::values(pf) <- NA
  paint_file <- file.path(label_dir, 'paint_in_prog.tif')
  raster::writeRaster(pf, paint_file, overwrite = TRUE)
  
  split_format <- shiny::tags$head(shiny::tags$style(htmltools::HTML(".shiny-split-layout > div { overflow: visible; }")))
  abs_style <- "background:white; padding: 20px 20px 20px 20px; border-radius: 5pt;"
  
  ui <- shiny::fillPage(
    leaflet::leafletOutput('m1_leaf', height = '100vh'),
    
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
        label = 'Map 1 quantiles',
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
                             shiny::h2("Labeling Tools"),
                             shiny::sliderInput(
                               inputId = 'thresh',
                               label = 'Similarity threshold',
                               ticks = FALSE,
                               value = 0.1,
                               min = 0,
                               max = 1,
                               step = 0.005
                             ),
                             shiny::selectInput(
                               inputId = 'label_class',
                               label = 'Select class to label:',
                               choices = names(label_key),
                               selected = names(label_key)[0]
                             ),
                             
                             shiny::actionButton(inputId = 'assign',
                                                 label = 'Label painted areas',
                                                 class = 'btn-primary'),
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
    
    
    output$m1_leaf <-
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
          )
      ) 
    
    
    shiny::observe({
      
      leaflet::leafletProxy('m1_leaf') %>%
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
    })
    
    shiny::observeEvent(c(input$u_1, input$u_2, input$u_3), {
     
      leaflet::leafletProxy('m1_leaf') %>%
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
    
    click_coords <- shiny::eventReactive(input$m1_leaf_click, {
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
    
    umap_vals <- shiny::eventReactive(input$m1_leaf_click, {
      if (is.null(click_coords()))
        return()
      vals <- raster::extract(umap_ras(), click_coords())
      print(vals)
      vals
    })
    
    shiny::observeEvent(input$img_sel, {
      pf <- umap_ras()
      raster::values(pf) <- NA
      raster::writeRaster(pf, paint_file, overwrite = TRUE)
      })
    
    shiny::observe({
      lab_file <- file.path(label_dir, basename(fname()))
      if(!file.exists(lab_file)){
        label_ras <- umap_ras()[[1]]
        raster::values(label_ras) <- NA
        raster::writeRaster(label_ras, lab_file, overwrite = TRUE)
      }
    })
    
    shiny::observeEvent(c(input$m1_leaf_click, input$thresh) , {
      if (is.na(umap_vals()[1]))
        return()
      udf <- data.frame(u1 = raster::values(umap_ras()[[1]]),
                        u2 = raster::values(umap_ras()[[2]]),
                        u3 = raster::values(umap_ras()[[3]]))
      
      dists <- RANN::nn2(data = umap_vals(),
                         query = umap_pts())$nn.dists %>% unlist()
      painted_ras <- raster::raster(paint_file)
      raster::values(painted_ras) <-
        ifelse(scales::rescale(dists) < input$thresh, 1, NA)
      
      raster::writeRaster(painted_ras, paint_file, overwrite = TRUE)
      
    })
    
    shiny::observeEvent(input$filter_noise, {
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
    
    shiny::observeEvent(input$assign,{
      lab_file <- file.path(label_dir, basename(fname()))
      label_ras <- raster::raster(lab_file)
      painted_ras <- raster::raster(paint_file)
      
      pix_to_class <- which(raster::values(painted_ras) == 1)
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
    
    shiny::observeEvent(
      c(
        input$m1_leaf_click,
        input$paint_col,
        input$paint_op,
        input$lab_op,
        input$filter_noise,
        input$fill_remainder,
        input$thresh,
        input$assign,
        input$m1
      ),{
        
        labs <- raster::raster(file.path(label_dir, basename(fname())))
        painted_ras <- raster::raster(paint_file)
        
        class_pal <- leaflet::colorNumeric(palette = label_cols, domain = label_key, na.color = 'transparent')
        
        leaflet::leafletProxy(map = 'm1_leaf') %>%
          leaflet::clearControls() %>%
          leaflet::clearGroup(group = 'Currently painted') %>%
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
    
    session$onSessionEnded(function() {
      if(file.exists(paint_file))
        file.remove(paint_file)
    })
    
  }
  
  shiny::shinyApp(ui = ui, server = server)
  
}