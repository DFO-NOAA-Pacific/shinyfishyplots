# biomass module

biomass_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
    uiOutput(ns("description_card")),
    
    withSpinner(uiOutput(ns("dbiPlotUI")), type = 3, size = 2, color.background = "#FFFFFFD0"), #dynamic height
    downloadButton(ns("downloadBiomass"), "Download Biomass Plot"),
    downloadButton(ns("downloadStanBiomass"), "Download Standardized Biomass Plot"))
}

biomass_Server <- function(id, all_dbi, region_names, input_region, surveys_selected, input_species) {
  moduleServer(
    id,
    function(input, output, session) {
      print("biomass module active")
      
      
      #### Conditional description card ###
      output$description_card <- renderUI({
        if (input_region() == "All regions") { # show card when "all regions" is selected
        accordion(
          open = NULL,
          accordion_panel(
            title = "Design-Based Biomass Indicies",
            card_body(
              tags$p("When", 
                     tags$strong("'All Regions'"),
                     " is selected, only standardized biomass indices are shown and can be viewed for multiple survey areas. Indices were standardized by dividing each survey’s values by its mean, setting the average to 1. See 'About the Data' in the 'Home' tab for information on the region/survey groupings and abbreviations." ))) )
          } else { #only show card when all regions NOT selected
        accordion(
          open = NULL,
          accordion_panel(
            title = "Design-Based Biomass Indicies",
            card_body(tags$div("These biomass indicies are design-based and may be calculated differently among science centers.", 
                               tags$strong("Not all surveys have yearly biomass estimates."),
                               " Indices were standardized by dividing each survey’s values by its mean, setting the average to 1. To compare standardized biomass estimations for different regions, select 'All Regions'. See 'About the Data' in the 'Home' tab for information on the region/survey groupings and abbreviations."))))}
      })
      
      
      #### Biomass plots and downloads ####
      output$dbiPlotUI <- renderUI({
        print("Renderplot running")
        print(input_region())
        ns <- session$ns
        
        if (input_region() == "All regions") {
          req(surveys_selected())
          plotOutput(ns("dbiPlot"), height = "500px")  # smaller for All regions, only one plot
        } else {
          plotOutput(ns("dbiPlot"), height = "900px")  # larger for stacked plots
        }
      })
      
      output$dbiPlot <- renderPlot({
        
        print("Renderplot running")
        req(input_species())
        req(input_region())
        
        
        if (input_region() == "All regions") { # messages for no region or species 
          req(surveys_selected())
          validate(#message for none selected
            need(!is.null(surveys_selected()) && length(surveys_selected()) > 0,
                 "Choose survey(s)"),
            need(input_species() != "" && input_species() != "None selected",
                 paste("Choose a species")))
          
          #create message if there is no DBI data for all selected surveys
          valid_dbi_surveys <- all_dbi |> 
            filter(common_name == input_species(), survey %in% surveys_selected())
          valid_dbi_surveys <-  unique(valid_dbi_surveys$survey)
          invalid_dbi_surveys <- setdiff(surveys_selected(), valid_dbi_surveys)
          
          validate(
            need(length(valid_dbi_surveys) > 0,
                 paste("No data for", input_species(), "in selected surveys"))
          )
          
          # Show a warning notif if some of selected surveys have no data
          if (length(invalid_dbi_surveys) > 0) {
            showNotification(
              paste("No data for", input_species(), "in:", paste(invalid_dbi_surveys, collapse = ", ")),
              type = "warning"
            )
          }
          
          plot_stan_dbi(input_species(), valid_dbi_surveys) # show only standardized plot if All regions selected
          
          
        } else if (input_region() == "Canada"| input_region() =="Aleutians/Bering Sea") { # options for regions with multiple surveys
          
          validate(#message for none selected
            need(!is.null(surveys_selected()) && length(surveys_selected()) > 0,
                 "Choose survey(s)"),
            need(input_species() != "" && input_species() != "None selected",
                 paste("Choose a species")))
          
          
          #create message if there is no DBI data for selected surveys
          valid_dbi_surveys <- all_dbi |> 
            filter(common_name == input_species(), survey %in% surveys_selected())
          valid_dbi_surveys <-  unique(valid_dbi_surveys$survey)
          invalid_dbi_surveys <- setdiff(surveys_selected(), valid_dbi_surveys)
          
          validate(
            need(length(valid_dbi_surveys) > 0,
                 paste("No data for", input_species(), "in selected surveys"))
          )
          
          # Show a warning notif if some of selected surveys have no data
          if (length(invalid_dbi_surveys) > 0) {
            showNotification(
              paste("No data for", input_species(), "in:", paste(invalid_dbi_surveys, collapse = ", ")),
              type = "warning"
            )
          }
          
          pdbi1 <- plot_dbi(input_species(), surveys_selected())
          pdbi2 <- plot_stan_dbi(input_species(), surveys_selected())
          pdbi1 + pdbi2 + plot_layout(ncol = 1) 
        } else { #when region has one survey
          
          validate( #message for none selected
            need(input_species() != "" && input_species() != "None selected",
                 paste("Choose a species")))
          
          #create message if there is no DBI data for selected surveys
          valid_dbi_surveys <- all_dbi |> 
            filter(common_name == input_species(), survey_group %in% region_names())
          valid_dbi_surveys <-  unique(valid_dbi_surveys$survey_group)
          validate(
            need(length(valid_dbi_surveys) > 0,
                 paste("No data for", input_species(), "in selected region/surveys")))
          
          pdbi1 <- plot_dbi(input_species(), region_names())
          pdbi2 <- plot_stan_dbi(input_species(), region_names())
          pdbi1 + pdbi2 + plot_layout(ncol = 1) 
        }
      })
      
      #download plots
      observe(
        if (input_region() == "All regions") {
          req(surveys_selected())
          #create message if there is no DBI data for all selected surveys
          valid_dbi_surveys <- all_dbi |> 
            filter(common_name == input_species(), survey %in% surveys_selected())
          valid_dbi_surveys <-  unique(valid_dbi_surveys$survey)
          
          # show only standardized plot if All regions selected
          output$downloadStanBiomass <- downloadHandler(
            filename = function() {paste0("stan_biomass_plots_", input_species(), ".png")},
            content = function(file) {ggsave(file, plot =  plot_stan_dbi(input_species(), valid_dbi_surveys), width = 10, device = "png")})
        } else if (input_region() == "Canada"| input_region() =="Aleutians/Bering Sea") { # not all regions, but more than one survey
          # show normal and standardized
          output$downloadBiomass <- downloadHandler(
            filename = function() {paste0("biomass_plot_", input_species(),"_", region_names(), ".png")},
            content = function(file) {ggsave(file, plot =  plot_dbi(input_species(), surveys_selected()), width = 10, device = "png")})
          output$downloadStanBiomass <- downloadHandler(
            filename = function() {paste0("stan_biomass_plot_", input_species(),"_", region_names(), ".png")},
            content = function(file) {ggsave(file, plot =  plot_stan_dbi(input_species(), surveys_selected()), width = 10, device = "png")})
        } else { # not all regions, only one survey per region
          output$downloadBiomass <- downloadHandler(
            filename = function() {paste0("biomass_plot_", input_species(),"_", region_names(), ".png")},
            content = function(file) {ggsave(file, plot =  plot_dbi(input_species(), region_names()), width = 10, device = "png")})
          output$downloadStanBiomass <- downloadHandler(
            filename = function() {paste0("stan_biomass_plot_", input_species(),"_", region_names(), ".png")},
            content = function(file) {ggsave(file, plot =  plot_stan_dbi(input_species(), region_names()), width = 10, device = "png")})
        }
      )
      
      
    }
  )
}