# Module - Depth


##### UI #####
depth_UI <- function(id) {
  ns <- NS(id)
  tagList(
  
    withSpinner(uiOutput(ns("dynamic_depth")), type = 3, size = 2, color.background = "#FFFFFFD0"),
    
    #download buttons
    downloadButton(ns("downloadAgeDepthPlot"), "Download age-depth plot"),
    downloadButton(ns("downloadLengthDepthPlot"), "Download length-depth plot"))

}


#### Server ####

depth_Server <- function(id, all_data, region_names, input_species) {
  moduleServer(
    id,
    function(input, output, session) {
      
      #### depth plots and downloads ####
      output$dynamic_depth <- renderUI({
        ns <- session$ns
        validate( #message for none selected
          need(input_species() != "" && input_species() != "None selected",
               paste("Choose a species")))
        width <- if (identical(region_names(), c("AK BSAI", "AK GULF", "PBS", "NWFSC"))) "100%" else "80%"
        tagList(plotOutput(ns("age_depthPlot"), width = width, height = "600px"),
                plotOutput(ns("length_depthPlot"), width = width, height = "600px")) })
      
      output$age_depthPlot <- renderPlot({
       
        validate( #message for none selected
          need(input_species() != "" && input_species() != "None selected",
               paste("Choose a species")))
        req(input_species() != "None selected")
        plot_age_depth(all_data, region_names(), input_species())})
      
      output$length_depthPlot <- renderPlot({
        
        validate( #message for none selected
          need(input_species() != "" && input_species() != "None selected",
               paste("Choose a species")))
        req(input_species() != "None selected")
        plot_length_depth(all_data, region_names(), input_species())})
      
      
      # data downloads
      # adaptive plot area sizing
      plot_width <- reactive({
        if (setequal(region_names(), c("AK BSAI", "AK GULF", "PBS", "NWFSC"))) {
          1200 / 96
        } else {
          800 / 96
        }
      })
      
      output$downloadAgeDepthPlot <- downloadHandler(
        filename = function() {paste0("age_depth_plot_", input_species(), ".png")},
        content = function(file) {ggsave(file, plot = plot_age_depth(all_data, region_names(), input_species()), width = plot_width(), device = "png")})
      
      output$downloadLengthDepthPlot <- downloadHandler(
        filename = function() {paste0("length_depth_plot_", input_species(), ".png")},
        content = function(file) {ggsave(file, plot = plot_length_depth(all_data, region_names(), input_species()), width = plot_width(), device = "png")})
      
    }
  )
}