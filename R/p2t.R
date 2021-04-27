p2t <- function(umap_dir, label_dir, ...) {
  umap_files <- list.files(umap_dir, full.names = TRUE)
  band_count <- raster::nlayers(raster::stack(umap_files[1]))
  band_choices <- as.numeric(seq_len(band_count))
  
  ui <- shiny::fillPage(
    leaflet::leafletOutput('rgb_leaf', height = '75%', width = '100%'),
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
    width = 60)
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
      raster::stack(b1,b2,b3)
    })
    
    ras_qts <- shiny::reactive({
      c(input$img_qt[1], input$img_qt[2])
    })
    
    output$rgb_leaf <- leaflet::renderLeaflet(leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = FALSE)) %>%
                                                leafem::addRasterRGB(rgb_ras(), 
                                                                     r = 1, 
                                                                     g = 2,
                                                                     b = 3,
                                                                     quantiles = ras_qts(), 
                                                                     max
                                                                       ))
    
  }
  
  shiny::shinyApp(ui = ui, server = server)
  
}