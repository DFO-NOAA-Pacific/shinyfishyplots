#Module - Data


#####UI ####
data_UI <- function(id) {
  ns <- NS(id)
  tagList(
  
    ## description card ####
    accordion(
      open = NULL,
      accordion_panel(
        title = "Sampling Overview",
        card_body("These plots display the number of biological measurements taken and tow effort for selected regions and species. 'Unread Ages' are the number of fishes with age structures collected but not analysed. For unrounded counts, use the data download below. "))),
    div(style = "overflow-x: scroll; min-width: 1200px;", #scrollable window
        withSpinner(plotOutput(ns("surveytable")), type = 3, size = 2, color.background = "#FFFFFFD0")),
    
    # download buttons
    downloadButton(ns("downloadSurveyTable"), "Download Survey Plot"),
    downloadButton(ns("downloadSurveyTibble"), "Download Survey Plot Data (Unrounded Counts)"),
    tags$div(Style = "margin-top: 50px;"),
    
   # download selection card and previews
    card(
      full_screen = FALSE,
      card_header("Data download options for selected region and species", style = "background-color: #d7d7d7;")),
    tableOutput(ns("demotable")),
    downloadButton(ns("downloadbio"), "Download biological data"),
    tags$div(Style = "margin-top: 50px;"),
    tableOutput(ns("vbtable")),
    downloadButton(ns("downloadvb"), "Download growth predictions"),
    tags$div(Style = "margin-top: 50px;"),
    tableOutput(ns("lwtable")),
    downloadButton(ns("downloadlw"), "Download length-weight predictions"),
    tags$div(Style = "margin-top: 50px;"),
    tableOutput(ns("maptable")),
    downloadButton(ns("downloadmap"), "Download density predictions"),
    tags$div(Style = "margin-top: 50px;"),
    tableOutput(ns("dbitable")),
    downloadButton(ns("downloaddbi"), "Download design-based biomass indicies"))
    
}


##### Server ####
data_Server <- function(id, all_dbi, all_data, lw_predictions, vb_predictions, predictions, region_names,input_species) {
  moduleServer(
    id,
    function(input, output, session) {
      
      #data downloading
      bio_subset <- reactive({
        all_data <- all_data |> select(-otosag_id)
        subset(all_data, common_name == input_species() & survey %in% region_names())
      })
      vb_subset <- reactive({
        subset(vb_predictions, common_name == input_species() & survey %in% region_names())
      })
      lw_subset <- reactive({
        subset(lw_predictions, common_name == input_species() & survey %in% region_names())
      })
      map_subset <- reactive({
        predictions <- predictions |> select(-sanity) |> select(-survey)
        subset(predictions, species == input_species() & subregion %in% region_names())
      })
      dbi_subset <- reactive({
        subset(all_dbi, common_name == input_species() & survey_group %in% region_names())
      })
      
      #### Data plots and downloads ####
      # Survey table
      output$surveytable <- renderPlot({
        validate( #message for none selected
          need(input_species() != "" && input_species() != "None selected",
               paste("Choose a species")))
        req(!(input_species() %in% c("None selected", "")))
        survey_table(all_data , region_names(), input_species(), form = 2, facet_all = TRUE)
      }, width = 1200,  height = function() {
        275 * length(region_names()) #dynamically change plot size based on how many are plotted
      })
      
      # Download counts
      observeEvent(
        output$downloadSurveyTable <- downloadHandler(
          filename = function() {paste0("SurveyCount_plot_", input_species(), ".png")},
          content = function(file) {ggsave(file, plot = survey_table(all_data , region_names(), input_species(), form = 2, facet_all = TRUE), width = 15, device = "png")}),
        output$downloadSurveyTibble <-  downloadHandler(
          filename = function() {paste0("SurveyCount_table_", input_species(), ".csv")},
          content = function(file) {write.csv(survey_table(all_data , region_names(), input_species(), form = 1), file)})
      )
      # Download biological data
      output$demotable <- renderTable({
        head(bio_subset(), n = 2)})
      output$downloadbio <- downloadHandler(
        filename = function() {paste0("biodata_", input_species(), ".csv")},
        content = function(file) {write.csv(bio_subset(), file)})
      
      # Download growth predictions
      output$vbtable <- renderTable({
        head(vb_subset(), n = 2)})
      output$downloadvb <- downloadHandler(
        filename = function() {paste0("growth_predictions_", input_species(), ".csv")},
        content = function(file) {write.csv(vb_subset(), file)})
      
      # Download map predictions
      output$maptable <- renderTable({
        #map_subset() <- map_subset() |> select(-sanity)
        head(map_subset(), n = 2)})
      output$downloadmap <- downloadHandler(
        filename = function() {paste0("density_predictions_", input_species(), ".csv")},
        content = function(file) {write.csv(map_subset(), file)})
      
      # Download LW predictions
      output$lwtable <- renderTable({
        head(lw_subset(), n = 2)})
      output$downloadlw <- downloadHandler(
        filename = function() {paste0("length_weight_predictions_", input_species(), ".csv")},
        content = function(file) {write.csv(lw_subset(), file)})
      
      # Download DBI 
      output$dbitable <- renderTable({
        head(dbi_subset(), n = 2)})
      output$downloaddbi <- downloadHandler(
        filename = function() {paste0("design_biomass_index_", input_species(), ".csv")},
        content = function(file) {write.csv(dbi_subset(), file)})
      
      
      }
  )
}