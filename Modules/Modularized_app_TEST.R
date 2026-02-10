# whole app


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


#### Source modlule scripts ####
source("home_module_TEST.R")
source("biomass_module_TEST.R")
source("agelength_module_TEST.R")
source("maps_module_TEST.R")
source("depth_module_TEST.R")
source("data_module_TEST.R")

###### UI #####
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
      div("A tool to visualize data from NOAA and DFO surveys.",
          style = "font-size:16px; color:black;"),
      div("Select a tab to get started!",
          style = "font-size:16px; color:black; font-weight: bold;")
    ),
    
    conditionalPanel( # additional selection menus for ALL REGIONS BIOMASS
      condition = "input.region == 'All regions' && input.tabs == 'Biomass'",
      checkboxGroupInput(
        inputId = "surveys_selected",
        label = "Select surveys",
        choices = c(
          "U.S. West Coast", "Hectate Strait" = "SYN HS", "Queen Chatlotte Sound" = "SYN QCS", "Haida Gwaii" = "SYN WCHG", "Vancouver Island" = "SYN WCVI",
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
        choices = c("Hectate Strait" = "SYN HS", "Queen Chatlotte Sound" = "SYN QCS", "Haida Gwaii" = "SYN WCHG", "Vancouver Island" = "SYN WCVI" )
      )
    ), 
  ),

  #### Tab Modules ####
  tabsetPanel(
    id = "tabs",
    tabPanel("Home", home_UI("home")),
    tabPanel("Biomass", biomass_UI("biomass")),
    tabPanel("Age and Length", agelength_UI("agelength")),
    tabPanel("Maps", maps_UI("maps")),
    tabPanel("Depth", depth_UI("depth")),
    tabPanel("Data", data_UI("data")))
  )
  
###### Server #####
  

server <- function(input, output, session) {
  
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
  
  
  #### CALL MODULE SERVERS ####
  home_Server("home")
  biomass_Server("biomass", all_dbi = all_dbi, region_names = region_names, surveys_selected = reactive(input$surveys_selected), input_region = reactive(input$region),
                 input_species = reactive(input$species))
  agelength_Server("agelength",all_data = all_data, region_names = region_names,
                                  input_species = reactive(input$species), lw_predictions = lw_predictions, vb_predictions = vb_predictions)
  maps_Server("maps",predictions = predictions, region_names = region_names,
              input_species = reactive(input$species))
  depth_Server("depth", all_data = all_data,
               region_names = region_names,
               input_species = reactive(input$species))
  data_Server("data", all_data = all_data,lw_predictions = lw_predictions, vb_predictions = vb_predictions, predictions = predictions, region_names = region_names,
              input_species = reactive(input$species))
  
}

#### run app ####
shinyApp(ui, server)
