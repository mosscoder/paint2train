p2t <- function(umap_dir, label_dir, ...) {
  umap_files <- list.files(umap_dir, full.names = TRUE)
  
  ui <- shiny::fillPage(
    leaflet::leafletOutput('rgb_leaf', height = '100%', width = '100%'),
    shiny::absolutePanel(
      shiny::selectInput(
        inputId = 'img_sel',
        label = 'Select image',
        choices = basename(umap_files)
      ),
      top = 10,
      left = 10
    )
  )
  
  server <- function(input, output) {
    
    fname <- shiny::reactive({
     file_ind <- which(basename(umap_files) == input$img_sel)
     umap_files[file_ind]
    })
    
    rgb_ras <- shiny::reactive({
      raster::stack(fname())[[3:5]]
    })
    
    output$rgb_plot <- shiny::renderPlot(raster::plotRGB(rgb_ras()))
    output$rgb_leaf <- leaflet::renderLeaflet(leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = FALSE)) %>%
                                                leafem::addRasterRGB(rgb_ras(), ...))
    
  }
  
  shiny::shinyApp(ui = ui, server = server)
  
}