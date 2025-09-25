# Load packages
library(shiny)
library(bslib)
library(surveyjoin)
library(sdmTMB)
library(fishyplots) #devtools::install_github("DFO-NOAA-Pacific/fishyplots")
library(ggplot2)
library(dplyr)
library(patchwork)
library(shinycssloaders)


##### Data #####

# Load biological data
data(nwfsc_bio)
nwfsc_bio <- nwfsc_bio |>  filter(!common_name == "walleye pollock") #only one walleye, breaks some functions
data(afsc_bio)
data(pbs_bio)
akbsai <- afsc_bio |> filter(survey == "AK BSAI")
akgulf <- afsc_bio |> filter(survey == "AK GULF")
all_data <- bind_rows(afsc_bio, nwfsc_bio, pbs_bio)

# Load prediction data
data(vb_predictions)
data(predictions_afsc)
data(predictions_nwfsc)
data(predictions_pbs)
predictions <- bind_rows(predictions_afsc, predictions_pbs, predictions_nwfsc)
predictions <- predictions |>
  mutate(subregion = case_when(
    region == "NWFSC" ~ "NWFSC",
    region == "PBS" ~ "PBS",
    region == "Gulf of Alaska Bottom Trawl Survey" ~ "AK GULF",
    TRUE ~ "AK BSAI"
  ))

# Load biomass data
data("all_dbi")

#### below code for only species with data available in region
# # Define overlap species
# overlap <- all_data |>
#   distinct(common_name, region) |>
#   count(common_name, name = "n") |>
#   filter(n >= 2) |>
#   pull(common_name)

# Create species list for each region
# spp_list <- list(
#   "Aleutians/Bering Sea" = sort(unique(akbsai$common_name)),
#   "Gulf of Alaska" = sort(unique(akgulf$common_name)),
#   "US West Coast" = sort(unique(nwfsc_bio$common_name)),
#   "Canada" = sort(unique(pbs_bio$common_name)),
#   "All regions" = sort(overlap)
# )

# species list for region selections (all the same)
spp_list <- list(
"Aleutians/Bering Sea" = sort(unique(all_data$common_name)),
"Gulf of Alaska" = sort(unique(all_data$common_name)),
"US West Coast" = sort(unique(all_data$common_name)),
"Canada" = sort(unique(all_data$common_name)),
"All regions" = sort(unique(all_data$common_name))
)

##### Define User Interface #####
ui <- page_sidebar(
  #### Sidebar + Formatting ####
  title = div(
    "Pacific Survey Explorer",
    style = "background-color:#2C3E79; color:white; font-weight:bold; 
             padding:12px; font-size:1.5em;"
  ),

  
  sidebar_width = 2,
  
  theme = bs_theme(
    bg = "#FFF",
    fg = "#101010",
    primary = "#2C3E79",
    secondary = "#2C3E79"),
  tags$style(
  
  HTML("
   .sidebar {
      background-color: #d8d8d8 !important;
      color: black !important;
      padding: 15px;
   }
  /* Inactive tabs */
  .nav-tabs .nav-link {
  color: #2C3E79 !important;  /* blue */
  }
  
  /* Active tab */
  .nav-tabs .nav-link.active {
  color: #D9B15C !important; 
  }
  
  /* format collapsible cards */
  .accordion-item {
    border: 1px solid #ddd;
    border-radius: 12px !important;
    margin-bottom: 10px;
    overflow: hidden; 
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
  }
  .accordion-button {
    background-color: #d7d7d7 !important;
    color: black !important;
    border-radius: 12px 12px 0 0 !important;
  }
  .accordion-button:not(.collapsed) {
    background-color: #d7d7d7 !important;
    color: black !important;
    border-radius: 12px 12px 0 0 !important;
  }
  .accordion-body {
    background-color: white;
    border-radius: 0 0 12px 12px !important;
  }
    ")),
  
  # Sidebar: show only if not on "Home" tab
  sidebar = tagList(
    
    conditionalPanel( # default sidebar for most tabs
      condition = "input.tabs != 'Home'",
      div("A tool to visualize data from NOAA and DFO surveys.",
          style = "font-size:16px; color:black;"),
      radioButtons(
        inputId = "region",
        label = "Choose a region",
        choices = list("All regions", "Aleutians/Bering Sea", "Gulf of Alaska", "Canada", "US West Coast"),
        selected = "All regions"
      ),
      selectInput(
        "species",
        label = "Choose a species",
        choices = NULL
      )
    ),
    
    conditionalPanel( # Home tab sidebar : no selections
      condition = "input.tabs == 'Home'",
      div("A tool to visualize data from NOAA and DFO scientific surveys.",
          style = "font-size:16px; color:black;"),
      div("Select a tab to get started.",
          style = "font-size:16px; color:black; font-weight: bold;")
    ),
    
    conditionalPanel( # additional selection menus for ALL REGIONS BIOMASS
      condition = "input.region == 'All regions' && input.tabs == 'Biomass'",
      checkboxGroupInput(
        inputId = "surveys_selected",
        label = "Select surveys",
        choices = c(
          "U.S. West Coast", "Hectate Strait" = "SYN HS", "Queen Chatlotte Sound" = "SYN QCS", "Haida Gwaii" = "SYN WCHG", "West Coast Vancouver Island" = "SYN WCVI",
          "Gulf of Alaska" = "U.S. Gulf of Alaska",
          "Aleutian Islands" = "U.S. Aleutian Islands",
          "Eastern Bering Slope" = "U.S. Eastern Bering Sea Slope",
          "Eastern Bering and NW" = "U.S. Eastern Bering Sea Standard Plus NW Region",
          "Northern Bering" = "U.S. Northern Bering Sea" )
      )
    ), 
    conditionalPanel( # additional selection menus for AK BSAI BIOMASS
      condition = "input.region == 'Aleutians/Bering Sea' && input.tabs == 'Biomass'",
      checkboxGroupInput(
        inputId = "surveys_selected",
        label = "Select surveys)",
        choices = c(
          "Aleutian Islands" = "U.S. Aleutian Islands",
          "Eastern Bering Slope" = "U.S. Eastern Bering Sea Slope",
          "Eastern Bering and NW" = "U.S. Eastern Bering Sea Standard Plus NW Region",
          "Northern Bering" = "U.S. Northern Bering Sea")
      )
    ), 
    conditionalPanel( # additional selection menus for CANADA BIOMASS
      condition = "input.region == 'Canada' && input.tabs == 'Biomass'",
      checkboxGroupInput(
        inputId = "surveys_selected",
        label = "Select surveys",
        choices = c("Hectate Strait" = "SYN HS", "Queen Chatlotte Sound" = "SYN QCS", "Haida Gwaii" = "SYN WCHG", "West Coast Vancouver Island" = "SYN WCVI" )
      )
    ), 
  ),
  
  #### Home tab  ####
  tabsetPanel(
    id = "tabs",
    tabPanel("Home",
             card(
               full_screen = FALSE,
               card_header("About this tool", style = "background-color: #d7d7d7;"),
               card_body(
                 tags$p("This interactive app serves as a coastwide synopsis for groundfish in the northeast Pacific Ocean,
                   providing information on fish biology, predicted spatial distributions, and biomass index trends. This tool is intended to support managers, scientists, collaborators, and others to explore available data for monitoring and management of marine ecosystems and resources.")
               ) ),
             card(
               full_screen = FALSE,
               card_header("About the data", style = "background-color: #d7d7d7;"),
               card_body(
                 tags$p("Our data come from", tags$strong("fishery-independent trawl surveys "),
                 "conducted by the ",tags$strong("National Oceanic and Atmospheric Administration (NOAA) "),
                 "and ", tags$strong("Fisheries and Oceans Canada (DFO)"),", compiled from NOAA's Alaska Fisheries Science Center (AFSC) and Northwest Fisheries Science Center (NWFSC), and DFO's Pacific Biological Station (PBS).
                 For each survey region (U.S. West Coast, British Columbia, Alaska), we identified the top 20 species with respect to total biomass in all survey years. 
                        We also added the top 20 species that have been ranked as occurring in multiple areas, as part of the ",
                        HTML(' <a href = "https://doi.org/10.5281/zenodo.10031852" target = "_self" >surveyjoin</a> package.'),
                 "See citations for more information on the surveys and data collection."),
                 tags$b(tags$u("Regions and Associated Surveys")),
                 tags$div(
                   tags$strong("Aleutians/Bering Sea (AFSC):"), tags$br(),
                   tags$div(style = "margin-left: 1em;", "Aleutian Islands"),
                   tags$div(style = "margin-left: 1em;", "U.S. Eastern Bering Sea Slope"),
                   tags$div(style = "margin-left: 1em;", "U.S. Eastern Bering Sea Standard Plus NW Region"),
                   tags$div(style = "margin-left: 1em;", "U.S. Northern Bering Sea"),
                   tags$br(),
                   tags$strong("Gulf of Alaska (AFSC):"), tags$br(),
                   tags$div(style = "margin-left: 1em;", "Gulf of Alaska"),
                   tags$br(),
                   tags$strong("Canada (PBS):"), tags$br(),
                   tags$div(style = "margin-left: 1em;", "Synoptic Hecate Strait (SYN HS)"),
                   tags$div(style = "margin-left: 1em;", "Synoptic Queen Charlotte Sound (SYN QCS)"),
                   tags$div(style = "margin-left: 1em;", "Synoptic West Coast Vancouver Island (SYN WCHG)"),
                   tags$div(style = "margin-left: 1em;", "Synoptic West Coast Haida Gwaii (SYN WCVI)"),
                   tags$br(),
                   tags$strong("US West Coast (NWFSC):"), tags$br(),
                   tags$div(style = "margin-left: 1em;", "U.S. West Coast")
                 )
               )
             ),
             card(
               full_screen = FALSE,
               card_header("Code and Acknowledgements", style = "background-color: #d7d7d7;"),
               card_body(
                 tags$p("This app uses plotting functions from the ",
                        tags$a(href = "https://doi.org/10.5281/zenodo.15932836", "fishyplots", target = "_self"), 
                        " package, authored by Callie Murakami and Zoe Khan during their 2025 summer internship. 
                    The code is heavily inspired by the Fisheries and Oceans Canada ",
                        tags$a(href = "https://github.com/pbs-assess/gfsynopsis", "BC Groundfish Data Synopsis Report", target = "_self"),
                        " and builds off an ",
                        tags$a(href = "https://github.com/DFO-NOAA-Pacific/gfsynopsis-noaa", "initial version", target = "_self"),
                        " from 2024.")
               )),
             card(
               full_screen = FALSE,
               card_header("Feedback", style = "background-color: #d7d7d7;"),
               card_body(
                 tags$p("Have a question or found a bug? Please ", 
                        HTML(' <a href = "https://github.com/DFO-NOAA-Pacific/shinyfishyplots/issues" target = "_self" >report here</a>.')
                 )
               )),
             card(full_screen = FALSE,
                  card_header("Data References", style = "background-color: #d7d7d7;"),
                  card_body(
                      tags$strong("Aleutians Islands Bottom Trawl Survey"),
                      tags$ul(tags$li("Von Szalay PG, Raring NW, Siple MC, Dowlin AN, Riggle BC, and Laman EA. 2023. Data Report: 2022 Aleutian Islands bottom trawl survey. U.S. Dep. Commer. DOI: ",
                 tags$a("10.25923/85cy-g225", 
                        href = "https://doi.org/10.25923/85cy-g225", 
                        target = "_blank"))),
                      tags$strong("Gulf of Alaska Bottom Trawl Survey"), 
                      tags$ul(tags$li("Siple MC, von Szalay PG, Raring NW, Dowlin AN, Riggle BC. 2024. Data Report: 2023 Gulf of Alaska bottom trawl survey. DOI: ",
                 tags$a("10.25923/GBB1-X748", 
                        href = "https://doi.org/10.25923/GBB1-X748", 
                        target = "_blank"))),
                      tags$strong("Eastern & Northern Bering Sea Crab/Groundfish Bottom Trawl Surveys"), 
                      tags$ul(
                        tags$li("Zacher LS, Richar JI, Fedewa EJ, Ryznar ER, Litzow MA. 2023. The 2023 Eastern Bering Sea Continental Shelf Trawl Survey: Results for Commercial Crab Species. U.S. Dep. Commer, 213 p."),
                        tags$li("Markowitz EH, Dawson EJ, Wassermann S, Anderson AB, Rohan SK, Charriere BK, Stevenson DE. 2024. Results of the 2023 eastern and northern Bering Sea continental shelf bottom trawl survey of groundfish and invertebrate fauna. U.S. Dep. Commer.")),
                        tags$strong("Eastern Bering Sea Slope Bottom Trawl Survey"),
                        tags$ul(
                          tags$li("Hoff GR. 2016. Results of the 2016 eastern Bering Sea upper continental slope survey of groundfishes and invertebrate resources. U.S. Dep. Commer. DOI: ",
        tags$a("10.7289/V5/TM-AFSC-339", 
               href = "https://doi.org/10.7289/V5/TM-AFSC-339", 
               target = "_blank"))),
                      tags$strong("Fisheries and Oceans Canada Synoptic Bottom Trawl Surveys"), 
                      tags$ul(
                        tags$li(
                          "Anderson SC, Keppel EA, Edwards AM. 2019. ",
                          HTML('<a href="https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2019/2019_041-eng.html" target="_blank">
       A reproducible data synopsis for over 100 species of British Columbia groundfish</a>.'),
                          " DFO Can. Sci. Advis. Sec. Res. Doc. 2019/041, vii + 321 p."
                        ),
                        tags$li("Sinclair A, Schnute J, Haigh R, Starr P, Stanley R, Fargo J, Workman G. 2003. Feasibility of Multispecies Groundfish Bottom Trawl Surveys on the BC Coast. DFO Canadian Science Advisory Secretariat (CSAS) Research Document, 2003/049."),
                        tags$li(HTML("Nottingham MK, Williams DC, Wyeth MR, Olsen N. 2017. <a href='https://publications.gc.ca/site/eng/9.848701/publication.html' target='_blank'>Summary of the West Coast Vancouver Island synoptic bottom trawl survey, May 28 – June 21, 2014</a>. DFO Can. Manuscr. Rep. Fish. Aquat. Sci. 2017/3140, viii + 55 p, Nanaimo.")),
                        tags$li("Williams DC, Nottingham MK, Olsen N, Wyeth MR. 2018a. Summary of the Queen Charlotte Sound synoptic bottom trawl survey, July 6 – August 8, 2015. DFO Can. Manuscr. Rep. Fish. Aquat. Sci. 3136, viii + 64 p, Nanaimo."),
                        tags$li("Williams DC, Nottingham MK, Olsen N, Wyeth MR. 2018b. Summary of the West Coast Haida Gwaii synoptic bottom trawl survey, August 25 – October 2, 2014. DFO Can. Manuscr. Rep. Fish. Aquat. Sci. 2018/3134, viii + 42 p, Nanaimo."),
                        tags$li("Wyeth MR, Olsen N, Nottingham MK, Williams DC. 2018. Summary of the Hecate Strait synoptic bottom trawl survey, May 26 – June 22, 2015. DFO Can. Manuscr. Rep. Fish. Aquat. Sci. 2018/3126, viii + 55 p, Nanaimo.")),
                      tags$strong("USA West Coast Bottom Trawl Surveys"), 
                      tags$ul(
                        tags$li("Keller AA, Wallace JR, Methot RD. 2017. The Northwest Fisheries Science Center's West Coast Groundfish Bottom Trawl Survey: history, design, and description. DOI: ",
                          tags$a("10.7289/V5/TM-NWFSC-136", 
                            href = "https://doi.org/10.7289/V5/TM-NWFSC-136", 
                            target = "_blank")))
                  ))
             ),
    
  #### Biomass tab ####
    tabPanel("Biomass",
             conditionalPanel( #only show card when all regions selected
               condition = "input.region == 'All regions'",
               accordion(
                 open = NULL,
                 accordion_panel(
                   title = "Design-Based Biomass Indicies",
               card_body(
                 tags$p("When", 
                 tags$strong("'All Regions'"),
                 " is selected, only standardized biomass indices are shown and can be viewed for multiple survey areas. Indices were standardized by dividing each survey's values by its mean, setting the average to 1. See ",
                 actionLink("go_home_1", "'About the Data' in the 'Home' tab", style = "color: #2C3E79; text-decoration: underline;"),
                 " for information on the region/survey groupings and abbreviations." ))) )),

             conditionalPanel( #only show card when all regions NOT selected
               condition = "input.region != 'All regions'",
               accordion(
                 open = NULL,
                 accordion_panel(
                   title = "Design-Based Biomass Indicies",
               card_body(tags$div("These biomass indicies are design-based and may be calculated differently among science centers.",
                                  tags$strong("Not all surveys have yearly biomass estimates."),
                                  " Indices were standardized by dividing each survey's values by its mean, setting the average to 1. To compare standardized biomass estimations for different regions, select 'All Regions'. See ",
                                  actionLink("go_home_2", "'About the Data' in the 'Home' tab", style = "color: #2C3E79; text-decoration: underline;"),
                                  " for information on the region/survey groupings and abbreviations."))))),
             
             withSpinner(uiOutput("dbiPlotUI"), type = 3, size = 2, color.background = "#FFFFFFD0"), #dynamic height
             downloadButton("downloadBiomass", "Download Biomass Plot"),
             downloadButton("downloadStanBiomass", "Download Standardized Biomass Plot")), 
  
  #### Age and Length tab ####
    tabPanel("Age and length",
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
             downloadButton("downloadLengthFreq", "Download length frequency plot")),
  #### Maps tab ####
    tabPanel("Maps",
             accordion(
               open = NULL,
               accordion_panel(
                 title = "Disclaimer",
               card_body(
                 tags$p("Maps were made using land data from ",
                 HTML(' <a href = "https://doi.org/10.32614/CRAN.package.rnaturalearth" target = "_self" >rnaturalearth</a>.'),
                 "Spatial predictions were generated using a model-based approach applied to the most recent year of survey data with ",
                 HTML(' <a href = "https://pbs-assess.github.io/sdmTMB/", "sdmTMB", target = "_self" >sdmTMB</a>.'),
                 "Because of differences in years or model settings, these results may not capture true distributions and may differ from other presentations. 
                These maps are exploratory and should not be used as definitive sources for management decisions.")
               )
             )),
             withSpinner(uiOutput("dynamicMap"), type = 3, size = 2, color.background = "#FFFFFFD0"), #dynamic height
             downloadButton("downloadMapPlot", "Download map")),
  #### Depth tab ####
    tabPanel("Depth",
             withSpinner(uiOutput("dynamic_depth"), type = 3, size = 2, color.background = "#FFFFFFD0"),
             downloadButton("downloadAgeDepthPlot", "Download age-depth plot"),
             downloadButton("downloadLengthDepthPlot", "Download length-depth plot")),
  #### Data tab ####
    tabPanel("Data",
             accordion(
               open = NULL,
               accordion_panel(
                 title = "Sampling Overview",
               card_body("These plots display the number of biological measurements taken and tow effort for selected regions and species. 'Unread Ages' are the number of fishes with age structures collected but not analysed. For unrounded counts, use the data download below. "))),
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
  )
)

##### Define Server #####
server <- function(input, output, session) {
  #### Navigation handlers ####
  # Handle actionLink clicks to navigate to Home tab
  observeEvent(input$go_home_1, {
    updateTabsetPanel(session, "tabs", selected = "Home")
  })

  observeEvent(input$go_home_2, {
    updateTabsetPanel(session, "tabs", selected = "Home")
  })

  #### species selection ####
  # Dynamic species selection based on region
  region_names <- reactive({
    switch(input$region,
           "US West Coast" = "NWFSC", "Canada" = "PBS", "Aleutians/Bering Sea" = "AK BSAI", "Gulf of Alaska" = "AK GULF", "All regions" = c("AK BSAI", "AK GULF", "PBS", "NWFSC"))
  })
  observeEvent(input$region, { 
  region_species <- spp_list[[input$region]]
  # Check if currently selected species is also present in the newly selected region:
  current_spp <- input$species
  if (!is.null(current_spp) && current_spp %in% region_species) {
    selected_species <- current_spp
  } else {
    selected_species <- "None selected"
  }
  updateSelectInput(
    session,
    "species",
    choices = c("None selected", region_species),
    selected = selected_species
  )
  })
  # FOR DBI SURVEY SELECTIONS
  #reset survey selections when going to new region
  observeEvent(input$region, {
    updateCheckboxGroupInput(
      session, "surveys_selected",
      selected = character(0)) })

    
  #### Data Downloads ####
  # Dynamic subsetting for downloading data
  bio_subset <- reactive({
    all_data <- all_data |> select(-otosag_id)
    subset(all_data, common_name == input$species & survey %in% region_names())
  })
  vb_subset <- reactive({
    subset(vb_predictions, common_name == input$species & survey %in% region_names())
  })
  map_subset <- reactive({
    predictions <- predictions |> select(-sanity) |> select(-survey)
    subset(predictions, species == input$species & subregion %in% region_names())
  })
  lw_subset <- reactive({
    subset(lw_predictions, common_name == input$species & survey %in% region_names())
  })
  dbi_subset <- reactive({
    subset(all_dbi, common_name == input$species & survey_group %in% region_names())
  })
  
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
    validate( #message for none selected
      need(input$species != "" && input$species != "None selected",
           paste("Choose a species")))
    req(input$species != "None selected")
    plotOutput("modelPlot", height = map_height1())
    })
  
  output$modelPlot <- renderPlot({
    validate( #message for none selected
      need(input$species != "" && input$species != "None selected",
           paste("Choose a species")))
    req(input$species != "None selected")
    fishmap(predictions, region_names(), input$species)})
  
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
    filename = function() {paste0("map_", input$species, ".png")},
    content = function(file) {ggsave(file, plot = fishmap(predictions, region_names(), input$species), height = map_height2(), device = "png")})
  
  #### length, age, growth plots and downloads ####
  output$dynamic_agelength <- renderUI({
    width <- if (identical(region_names(), c("AK BSAI", "AK GULF", "PBS", "NWFSC"))) "100%" else "65%"
    plotOutput("agelengthPlot", width = width, height = "1250px")})
  
  output$agelengthPlot <- renderPlot({
    validate( #message for none selected
      need(input$species != "" && input$species != "None selected",
           paste("Choose a species")))
    req(input$species != c("None selected", ""))
    # Growth plot
    p1 <- plot_growth(all_data, region_names(), input$species) 
    # Length - weight
    p2 <- length_weight(all_data, region_names(), input$species, subset = TRUE)
    # Average Lengths
    p3 <- length_ts(all_data, region_names(), input$species)
    # Age frequency
    p4 <- age_frequency(all_data, region_names(), input$species, cutoff = 0.75)
    # Length frequency
    p5 <- length_frequency(all_data, region_names(), input$species, time_series = TRUE)
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
    filename = function() {paste0("growth_plot_", input$species, ".png")},
    content = function(file) {ggsave(file, plot = plot_growth(all_data, region_names(), input$species), width = plot_width(), device = "png")})
  
  output$downloadLW <- downloadHandler(
    filename = function() {paste0("lw_plot_", input$species, ".png")},
    content = function(file) {ggsave(file, plot = length_weight(all_data, region_names(), input$species, subset = TRUE), width = plot_width(), device = "png")})
  
  output$downloadLTS <- downloadHandler(
    filename = function() {paste0("avglengths_plot_", input$species, ".png")},
    content = function(file) {ggsave(file, plot = length_ts(all_data, region_names(), input$species), width = plot_width(), device = "png")})
  
  output$downloadAgeFreq <- downloadHandler(
    filename = function() {paste0("agefrequency_plot_", input$species, ".png")},
    content = function(file) {ggsave(file, plot = age_frequency(all_data, region_names(), input$species, cutoff = 0.75), width = plot_width(), device = "png")})
  
  output$downloadLengthFreq <- downloadHandler(
    filename = function() {paste0("lengthfrequency_plot_", input$species, ".png")},
    content = function(file) {ggsave(file, plot = length_frequency(all_data, region_names(), input$species, time_series = TRUE), width = plot_width(), device = "png")})
  
  #### depth plots and downloads ####
  output$dynamic_depth <- renderUI({
    validate( #message for none selected
      need(input$species != "" && input$species != "None selected",
           paste("Choose a species")))
    width <- if (identical(region_names(), c("AK BSAI", "AK GULF", "PBS", "NWFSC"))) "100%" else "80%"
    tagList(plotOutput("age_depthPlot", width = width, height = "600px"),
    plotOutput("length_depthPlot", width = width, height = "600px")) })
  
  output$age_depthPlot <- renderPlot({
    validate( #message for none selected
      need(input$species != "" && input$species != "None selected",
           paste("Choose a species")))
    req(input$species != "None selected")
    plot_age_depth(all_data, region_names(), input$species)})
  
  output$length_depthPlot <- renderPlot({
    validate( #message for none selected
      need(input$species != "" && input$species != "None selected",
           paste("Choose a species")))
    req(input$species != "None selected")
    plot_length_depth(all_data, region_names(), input$species)})
  
  output$downloadAgeDepthPlot <- downloadHandler(
    filename = function() {paste0("age_depth_plot_", input$species, ".png")},
    content = function(file) {ggsave(file, plot = plot_age_depth(all_data, region_names(), input$species), width = plot_width(), device = "png")})
  
  output$downloadLengthDepthPlot <- downloadHandler(
    filename = function() {paste0("length_depth_plot_", input$species, ".png")},
    content = function(file) {ggsave(file, plot = plot_length_depth(all_data, region_names(), input$species), width = plot_width(), device = "png")})
  
  #### Biomass plots and downloads ####
  output$dbiPlotUI <- renderUI({
    if (input$region == "All regions") {
      plotOutput("dbiPlot", height = "500px")  # smaller for All regions, only one plot
    } else {
      plotOutput("dbiPlot", height = "900px")  # larger for stacked plots
    }
  })
  
  output$dbiPlot <- renderPlot({
    if (input$region == "All regions") { # messages for no region or species 
      validate(#message for none selected
        need(!is.null(input$surveys_selected) && length(input$surveys_selected) > 0,
             "Choose survey(s)"),
        need(input$species != "" && input$species != "None selected",
             paste("Choose a species")))
    
      #create message if there is no DBI data for all selected surveys
    valid_dbi_surveys <- all_dbi |> 
      filter(common_name == input$species, survey %in% input$surveys_selected)
    valid_dbi_surveys <-  unique(valid_dbi_surveys$survey)
    invalid_dbi_surveys <- setdiff(input$surveys_selected, valid_dbi_surveys)
    
    validate(
      need(length(valid_dbi_surveys) > 0,
           paste("No data for", input$species, "in selected surveys"))
    )
    
    # Show a warning notif if some of selected surveys have no data
    if (length(invalid_dbi_surveys) > 0) {
      showNotification(
        paste("No data for", input$species, "in:", paste(invalid_dbi_surveys, collapse = ", ")),
        type = "warning"
      )
    }
    
    plot_stan_dbi(input$species, valid_dbi_surveys) # show only standardized plot if All regions selected
    
    
  } else if (input$region == "Canada"| input$region =="Aleutians/Bering Sea") { # options for regions with multiple surveys
    
    validate(#message for none selected
      need(!is.null(input$surveys_selected) && length(input$surveys_selected) > 0,
           "Choose survey(s)"),
      need(input$species != "" && input$species != "None selected",
           paste("Choose a species")))
    
    
    #create message if there is no DBI data for selected surveys
    valid_dbi_surveys <- all_dbi |> 
      filter(common_name == input$species, survey %in% input$surveys_selected)
    valid_dbi_surveys <-  unique(valid_dbi_surveys$survey)
    invalid_dbi_surveys <- setdiff(input$surveys_selected, valid_dbi_surveys)
    
    validate(
      need(length(valid_dbi_surveys) > 0,
           paste("No data for", input$species, "in selected surveys"))
    )
    
    # Show a warning notif if some of selected surveys have no data
    if (length(invalid_dbi_surveys) > 0) {
      showNotification(
        paste("No data for", input$species, "in:", paste(invalid_dbi_surveys, collapse = ", ")),
        type = "warning"
      )
    }
    
    pdbi1 <- plot_dbi(input$species, input$surveys_selected)
    pdbi2 <- plot_stan_dbi(input$species, input$surveys_selected)
    pdbi1 + pdbi2 + plot_layout(ncol = 1) 
  } else { #when region has one survey
    
    validate( #message for none selected
      need(input$species != "" && input$species != "None selected",
           paste("Choose a species")))
    
    #create message if there is no DBI data for selected surveys
    valid_dbi_surveys <- all_dbi |> 
      filter(common_name == input$species, survey_group %in% region_names())
    valid_dbi_surveys <-  unique(valid_dbi_surveys$survey_group)
    validate(
      need(length(valid_dbi_surveys) > 0,
           paste("No data for", input$species, "in selected region/surveys")))

    pdbi1 <- plot_dbi(input$species, region_names())
    pdbi2 <- plot_stan_dbi(input$species, region_names())
    pdbi1 + pdbi2 + plot_layout(ncol = 1) 
  }
  })
  
  #download plots
 observe(
   if (input$region == "All regions") {
    req(input$surveys_selected)
    #create message if there is no DBI data for all selected surveys
    valid_dbi_surveys <- all_dbi |> 
      filter(common_name == input$species, survey %in% input$surveys_selected)
    valid_dbi_surveys <-  unique(valid_dbi_surveys$survey)
    
    # show only standardized plot if All regions selected
    output$downloadStanBiomass <- downloadHandler(
      filename = function() {paste0("stan_biomass_plots_", input$species, ".png")},
      content = function(file) {ggsave(file, plot =  plot_stan_dbi(input$species, valid_dbi_surveys), width = 10, device = "png")})
  } else if (input$region == "Canada"| input$region =="Aleutians/Bering Sea") { # not all regions, but more than one survey
    # show normal and standardized
    output$downloadBiomass <- downloadHandler(
      filename = function() {paste0("biomass_plot_", input$species,"_", region_names(), ".png")},
      content = function(file) {ggsave(file, plot =  plot_dbi(input$species, input$surveys_selected), width = 10, device = "png")})
    output$downloadStanBiomass <- downloadHandler(
      filename = function() {paste0("stan_biomass_plot_", input$species,"_", region_names(), ".png")},
      content = function(file) {ggsave(file, plot =  plot_stan_dbi(input$species, input$surveys_selected), width = 10, device = "png")})
  } else { # not all regions, only one survey per region
    output$downloadBiomass <- downloadHandler(
      filename = function() {paste0("biomass_plot_", input$species,"_", region_names(), ".png")},
      content = function(file) {ggsave(file, plot =  plot_dbi(input$species, region_names()), width = 10, device = "png")})
    output$downloadStanBiomass <- downloadHandler(
      filename = function() {paste0("stan_biomass_plot_", input$species,"_", region_names(), ".png")},
      content = function(file) {ggsave(file, plot =  plot_stan_dbi(input$species, region_names()), width = 10, device = "png")})
  }
  )
  
  
 #### Data plots and downloads ####
  # Survey table
  output$surveytable <- renderPlot({
    validate( #message for none selected
      need(input$species != "" && input$species != "None selected",
           paste("Choose a species")))
    req(!(input$species %in% c("None selected", "")))
    survey_table(all_data , region_names(), input$species, form = 2, facet_all = TRUE)
  }, width = 1200,  height = function() {
    275 * length(region_names()) #dynamically change plot size based on how many are plotted
  })
  observeEvent(
  output$downloadSurveyTable <- downloadHandler(
    filename = function() {paste0("SurveyCount_plot_", input$species, ".png")},
    content = function(file) {ggsave(file, plot = survey_table(all_data , region_names(), input$species, form = 2, facet_all = TRUE), width = 15, device = "png")}),
  output$downloadSurveyTibble <-  downloadHandler(
    filename = function() {paste0("SurveyCount_table_", input$species, ".csv")},
    content = function(file) {write.csv(survey_table(all_data , region_names(), input$species, form = 1), file)})
  )
  
  # Download biological data
  output$demotable <- renderTable({
    head(bio_subset(), n = 2)})
  output$downloadbio <- downloadHandler(
    filename = function() {paste0("biodata_", input$species, ".csv")},
    content = function(file) {write.csv(bio_subset(), file)})
  
  # Download growth predictions
  output$vbtable <- renderTable({
    head(vb_subset(), n = 2)})
  output$downloadvb <- downloadHandler(
    filename = function() {paste0("growth_predictions_", input$species, ".csv")},
    content = function(file) {write.csv(vb_subset(), file)})
  
  # Download map predictions
  output$maptable <- renderTable({
    #map_subset() <- map_subset() |> select(-sanity)
    head(map_subset(), n = 2)})
  output$downloadmap <- downloadHandler(
    filename = function() {paste0("density_predictions_", input$species, ".csv")},
    content = function(file) {write.csv(map_subset(), file)})
  
  # Download LW predictions
  output$lwtable <- renderTable({
    head(lw_subset(), n = 2)})
  output$downloadlw <- downloadHandler(
    filename = function() {paste0("length_weight_predictions_", input$species, ".csv")},
    content = function(file) {write.csv(lw_subset(), file)})
  
  # Download DBI 
  output$dbitable <- renderTable({
    head(dbi_subset(), n = 2)})
  output$downloaddbi <- downloadHandler(
    filename = function() {paste0("design_biomass_index_", input$species, ".csv")},
    content = function(file) {write.csv(dbi_subset(), file)})
  
}

#### Run Shiny app! ####
shinyApp(ui = ui, server = server)


