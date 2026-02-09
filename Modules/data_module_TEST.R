## data tab module

data_UI <- function(id) {
  ns <- NS(id)
  tagList(
  
    accordion(
      open = NULL,
      accordion_panel(
        title = "Sampling Overview",
        card_body("These plots display the number of biological measurements taken and tow effort for selected regions and species. 'Unread Ages' are the number of fishes with age structures collected but not analysed. For completely unrounded counts, use the data download below. "))),
    div(style = "overflow-x: scroll; min-width: 1200px;", #scrollable window
        withSpinner(plotOutput("surveytable"), type = 3, size = 2, color.background = "#FFFFFFD0")),
    downloadButton("downloadSurveyTable", "Download Survey Plot"),
    downloadButton("downloadSurveyTibble", "Download Survey Plot Data (Unrounded Counts)"),
    tags$div(Style = "margin-top: 50px;"),
    card(
      full_screen = FALSE,
      card_header("Data download options for selected region and species", style = "background-color: #d7d7d7;")),
    tableOutput("demotable"),
    downloadButton("downloadbio", "Download biological data"),
    tags$div(Style = "margin-top: 50px;"),
    tableOutput("vbtable"),
    downloadButton("downloadvb", "Download growth predictions"),
    tags$div(Style = "margin-top: 50px;"),
    tableOutput("lwtable"),
    downloadButton("downloadlw", "Download length-weight predictions"),
    tags$div(Style = "margin-top: 50px;"),
    tableOutput("maptable"),
    downloadButton("downloadmap", "Download density predictions"),
    tags$div(Style = "margin-top: 50px;"),
    tableOutput("dbitable"),
    downloadButton("downloaddbi", "Download design-based biomass indicies"))
    
}

data_Server <- function(id, all_data, region_names, input_region, input_species) {
  moduleServer(
    id,
    function(input, output, session) {
      
      
      #### Data plots and downloads ####
      # Survey table
      output$surveytable <- renderPlot({
        validate( #message for none selected
          need(input_species != "" && input_species != "None selected",
               paste("Choose a species")))
        req(!(input_species %in% c("None selected", "")))
        survey_table(all_data , region_names(), input_species, form = 2, facet_all = TRUE)
      }, width = 1200,  height = function() {
        275 * length(region_names()) #dynamically change plot size based on how many are plotted
      })
      observeEvent(
        output$downloadSurveyTable <- downloadHandler(
          filename = function() {paste0("SurveyCount_plot_", input_species, ".png")},
          content = function(file) {ggsave(file, plot = survey_table(all_data , region_names(), input_species, form = 2, facet_all = TRUE), width = 15, device = "png")}),
        output$downloadSurveyTibble <-  downloadHandler(
          filename = function() {paste0("SurveyCount_table_", input_species, ".csv")},
          content = function(file) {write.csv(survey_table(all_data , region_names(), input_species, form = 1), file)})
      )
      # Download biological data
      output$demotable <- renderTable({
        head(bio_subset(), n = 2)})
      output$downloadbio <- downloadHandler(
        filename = function() {paste0("biodata_", input_species, ".csv")},
        content = function(file) {write.csv(bio_subset(), file)})
      
      # Download growth predictions
      output$vbtable <- renderTable({
        head(vb_subset(), n = 2)})
      output$downloadvb <- downloadHandler(
        filename = function() {paste0("growth_predictions_", input_species, ".csv")},
        content = function(file) {write.csv(vb_subset(), file)})
      
      # Download map predictions
      output$maptable <- renderTable({
        #map_subset() <- map_subset() |> select(-sanity)
        head(map_subset(), n = 2)})
      output$downloadmap <- downloadHandler(
        filename = function() {paste0("density_predictions_", input_species, ".csv")},
        content = function(file) {write.csv(map_subset(), file)})
      
      # Download LW predictions
      output$lwtable <- renderTable({
        head(lw_subset(), n = 2)})
      output$downloadlw <- downloadHandler(
        filename = function() {paste0("length_weight_predictions_", input_species, ".csv")},
        content = function(file) {write.csv(lw_subset(), file)})
      
      # Download DBI 
      output$dbitable <- renderTable({
        head(dbi_subset(), n = 2)})
      output$downloaddbi <- downloadHandler(
        filename = function() {paste0("design_biomass_index_", input_species, ".csv")},
        content = function(file) {write.csv(dbi_subset(), file)})
      
      
      }
  )
}