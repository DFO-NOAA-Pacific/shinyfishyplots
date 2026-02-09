# age and length module


agelength_UI <- function(id) {
  ns <- NS(id)
  tagList(
  
    #### Age and Length tab ####
   withSpinner(uiOutput("dynamic_agelength"), type = 3, size = 2, color.background = "#FFFFFFD0"), #object containing all plots 
   card(
     full_screen = FALSE,
     card_header("Model Info", style = "background-color: #d7d7d7;"),
     card_body(tags$div(tags$strong("Growth:"),"Von-Bertalanffy growth curve"),
               tags$div(tags$strong("Length-Weight:"), "Log-log regression"),
               tags$div("See 'Data' tab to download biological and prediction datasets used in these plots."))),
   downloadButton("downloadGrowth", "Download growth plot"),
   downloadButton("downloadLW", "Download length-weight plot"),
   downloadButton("downloadLTS", "Download average lengths plot"),
   downloadButton("downloadAgeFreq", "Download age frequency plot"),
   downloadButton("downloadLengthFreq", "Download length frequency plot"))

}

agelength_Server <- function(id, region_names, input_region, input_species, lw_predictions, vb_predictions) {
  moduleServer(
    id,
    function(input, output, session) {
      #data downloading
      bio_subset <- reactive({
        all_data <- all_data |> select(-otosag_id)
        subset(all_data, common_name == input_species & survey %in% region_names())
      })
      vb_subset <- reactive({
        subset(vb_predictions, common_name == input_species & survey %in% region_names())
      })
      lw_subset <- reactive({
        subset(lw_predictions, common_name == input_species & survey %in% region_names())
      })
    
      #### length, age, growth plots and downloads ####
      output$dynamic_agelength <- renderUI({
        width <- if (identical(region_names(), c("AK BSAI", "AK GULF", "PBS", "NWFSC"))) "100%" else "65%"
        plotOutput("agelengthPlot", width = width, height = "1250px")})
      
      output$agelengthPlot <- renderPlot({
        validate( #message for none selected
          need(input_species != "" && input_species != "None selected",
               paste("Choose a species")))
        req(input_species != c("None selected", ""))
        # Growth plot
        p1 <- plot_growth(all_data, region_names(), input_species) 
        # Length - weight
        p2 <- length_weight(all_data, region_names(), input_species, subset = TRUE)
        # Average Lengths
        p3 <- length_ts(all_data, region_names(), input_species)
        # Age frequency
        p4 <- age_frequency(all_data, region_names(), input_species, cutoff = 0.75)
        # Length frequency
        p5 <- length_frequency(all_data, region_names(), input_species, time_series = TRUE)
        # Combine with patchwork
        p1 + p2 + p3 + p4 + p5 + plot_layout(ncol = 1, heights = c(1, 1, 1, 1.5, 1))
      })
      
      plot_width <- reactive({
        if (setequal(region_names(), c("AK BSAI", "AK GULF", "PBS", "NWFSC"))) {
          1200 / 96
        } else {
          800 / 96
        }
      })
      
      output$downloadGrowth <- downloadHandler(
        filename = function() {paste0("growth_plot_", input_species, ".png")},
        content = function(file) {ggsave(file, plot = plot_growth(all_data, region_names(), input_species), width = plot_width(), device = "png")})
      
      output$downloadLW <- downloadHandler(
        filename = function() {paste0("lw_plot_", input_species, ".png")},
        content = function(file) {ggsave(file, plot = length_weight(all_data, region_names(), input_species, subset = TRUE), width = plot_width(), device = "png")})
      
      output$downloadLTS <- downloadHandler(
        filename = function() {paste0("avglengths_plot_", input_species, ".png")},
        content = function(file) {ggsave(file, plot = length_ts(all_data, region_names(), input_species), width = plot_width(), device = "png")})
      
      output$downloadAgeFreq <- downloadHandler(
        filename = function() {paste0("agefrequency_plot_", input_species, ".png")},
        content = function(file) {ggsave(file, plot = age_frequency(all_data, region_names(), input_species, cutoff = 0.75), width = plot_width(), device = "png")})
      
      output$downloadLengthFreq <- downloadHandler(
        filename = function() {paste0("lengthfrequency_plot_", input_species, ".png")},
        content = function(file) {ggsave(file, plot = length_frequency(all_data, region_names(), input_species, time_series = TRUE), width = plot_width(), device = "png")})
      
      }
  )
}