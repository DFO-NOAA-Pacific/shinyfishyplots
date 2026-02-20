# maps module

maps_UI <- function(id) {
  ns <- NS(id)
  tagList(
  
    accordion(
      open = NULL,
      accordion_panel(
        title = "Disclaimer",
        card_body(
          tags$p("Maps were made using land data from ",
                 HTML(' <a href = "https://doi.org/10.32614/CRAN.package.rnaturalearth" target = "" >rnaturalearth</a>.'),
                 "Spatial predictions were generated using a model-based approach applied to the most recent year of survey data with ",
                 HTML(' <a href = "https://pbs-assess.github.io/sdmTMB/", "sdmTMB", target = "_blank" >sdmTMB</a>.'),
                 "Because of differences in years or model settings, these results may not capture true distributions and may differ from other presentations. Notably, there are differences in gears between surveys (e.g., within the Aleutians/Bering Sea region) and these predictions are not adjusted for the resulting differences in catchability. Due to the large spatial scale, the maps are coarse approximations to broad spatial patterns and do not include habitat covariates which may be necessary for resolving fine-scale contrasts. These maps are exploratory and should not be used as definitive sources for management decisions or inference regarding population size at a specific location.")
        )
      )),
    withSpinner(uiOutput(ns("dynamicMap")), type = 3, size = 2, color.background = "#FFFFFFD0"), #dynamic height
    downloadButton(ns("downloadMapPlot"), "Download map"))
}


maps_Server <- function(id, predictions, region_names, input_species) {
  moduleServer(
    id,
    function(input, output, session) {
      #### Map plots and downloads ####
     
      map_height1 <- reactive({
        if (setequal(region_names(), c("AK BSAI", "AK GULF", "PBS", "NWFSC"))) {
          "1800px"
        } else if (region_names() %in% c("AK BSAI", "AK GULF")) {
          "450px"
        } else if (region_names() == "NWFSC") {
          "800px"
        } else if (region_names() == "PBS") {
          "600px"
        }
      })
      
      output$dynamicMap <- renderUI({
        ns <- session$ns
        validate( #message for none selected
          need(input_species() != "" && input_species() != "None selected",
               paste("Choose a species")))
        req(input_species() != "None selected")
        plotOutput(ns("modelPlot"), height = map_height1())
      })
      
      output$modelPlot <- renderPlot({
       
        validate( #message for none selected
          need(input_species() != "" && input_species() != "None selected",
               paste("Choose a species")))
        req(input_species() != "None selected")
        fishmap(predictions, region_names(), input_species())})
      
      map_height2 <- reactive({
        if (setequal(region_names(), c("AK BSAI", "AK GULF", "PBS", "NWFSC"))) {
          1800 / 96
        } else if (region_names() %in% c("AK BSAI", "AK GULF")) {
          450 / 96
        } else if (region_names() == "NWFSC") {
          800 / 96
        } else if (region_names() == "PBS") {
          600 / 96
        }
      })
      
      output$downloadMapPlot <- downloadHandler(
        filename = function() {paste0("map_", input_species(), ".png")},
        content = function(file) {ggsave(file, plot = fishmap(predictions, region_names(), input_species()), height = map_height2(), device = "png")})
      
      
      
    }
  )
}