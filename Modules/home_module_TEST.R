#Module testing - Home tab

# remember to wrap all IDs (anything in the namespace, anything in fishyplots that is named, like data) into ns() call in the UI area

library(shiny)
library(bslib)
library(surveyjoin)
library(sdmTMB)
library(fishyplots) #devtools::install_github("DFO-NOAA-Pacific/fishyplots")
library(ggplot2)
library(dplyr)
library(patchwork)
library(shinycssloaders)




#### Module UI ####

home_UI <- function(id) {
  ns <- NS(id)
  tagList(
               card(
                 full_screen = FALSE,
                 card_header("About this tool", style = "background-color: #d7d7d7;"),
                 card_body(
                   tags$p(tags$strong("Welcome!"),"This interactive app serves as a coastwide synopsis of fisheries in the northeast Pacific Ocean,
                   providing information on fish biology, predicted spatial distributions, and biomass. This tool is intended to support managers, scientists, collaborators, and others to explore available data for monitoring and management of marine ecosystems and resources.")
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
                          tags$a(href = "https://publications.gc.ca/site/eng/9.943594/publication.html", " 2023 data report", target = "_self"),
                          " and builds off an ",
                          tags$a(href = "https://github.com/DFO-NOAA-Pacific/gfsynopsis-noaa", "initial version", target = "_self"),
                          " from summer 2024.")
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
                    card_header("Citations", style = "background-color: #d7d7d7;"),
                    card_body(
                      tags$strong("Aleutians Islands Bottom Trawl Survey"),
                      tags$ul(tags$li("Von Szalay PG, Raring NW, Siple MC, Dowlin AN, Riggle BC, and Laman EA. 2023. Data Report: 2022 Aleutian Islands bottom trawl survey. U.S. Dep. Commer. DOI: 10.25923/85cy-g225.")),
                      tags$strong("Gulf of Alaska Bottom Trawl Survey"), 
                      tags$ul(tags$li("Siple MC, von Szalay PG, Raring NW, Dowlin AN, Riggle BC. 2024. Data Report: 2023 Gulf of Alaska bottom trawl survey. DOI: 10.25923/GBB1-X748.")),
                      tags$strong("Eastern & Northern Bering Sea Crab/Groundfish Bottom Trawl Surveys"), 
                      tags$ul(
                        tags$li("Zacher LS, Richar JI, Fedewa EJ, Ryznar ER, Litzow MA. 2023. The 2023 Eastern Bering Sea Continental Shelf Trawl Survey: Results for Commercial Crab Species. U.S. Dep. Commer, 213 p."),
                        tags$li("Markowitz EH, Dawson EJ, Wassermann S, Anderson AB, Rohan SK, Charriere BK, Stevenson DE. 2024. Results of the 2023 eastern and northern Bering Sea continental shelf bottom trawl survey of groundfish and invertebrate fauna. U.S. Dep. Commer.")),
                      tags$strong("Eastern Bering Sea Slope Bottom Trawl Survey"),
                      tags$ul(
                        tags$li("Hoff GR. 2016. Results of the 2016 eastern Bering Sea upper continental slope survey of groundfishes and invertebrate resources. U.S. Dep. Commer. DOI: 10.7289/V5/TM-AFSC-339.")),
                      tags$strong("Fisheries and Oceans Canada Synoptic Bottom Trawl Surveys"), 
                      tags$ul(
                        tags$li("Nottingham MK, Williams DC, Wyeth MR, Olsen N. 2017. Summary of the West Coast Vancouver Island synoptic bottom trawl survey, May 28 – June 21, 2014. DFO Can. Manuscr. Rep. Fish. Aquat. Sci. 2017/3140, viii + 55 p, Nanaimo."),
                        tags$li("Sinclair A, Schnute J, Haigh R, Starr P, Stanley R, Fargo J, Workman G. 2003. Feasibility of Multispecies Groundfish Bottom Trawl Surveys on the BC Coast. DFO Canadian Science Advisory Secretariat (CSAS) Research Document, 2003/049."),
                        tags$li("Williams DC, Nottingham MK, Olsen N, Wyeth MR. 2018a. Summary of the Queen Charlotte Sound synoptic bottom trawl survey, July 6 – August 8, 2015. DFO Can. Manuscr. Rep. Fish. Aquat. Sci. 3136, viii + 64 p, Nanaimo."),
                        tags$li("Williams DC, Nottingham MK, Olsen N, Wyeth MR. 2018b. Summary of the West Coast Haida Gwaii synoptic bottom trawl survey, August 25 – October 2, 2014. DFO Can. Manuscr. Rep. Fish. Aquat. Sci. 2018/3134, viii + 42 p, Nanaimo."),
                        tags$li("Wyeth MR, Olsen N, Nottingham MK, Williams DC. 2018. Summary of the Hecate Strait synoptic bottom trawl survey, May 26 – June 22, 2015. DFO Can. Manuscr. Rep. Fish. Aquat. Sci. 2018/3126, viii + 55 p, Nanaimo.")),
                      tags$strong("USA West Coast Bottom Trawl Surveys"), 
                      tags$ul(
                        tags$li("Keller AA, Wallace JR, Methot RD. 2017. The Northwest Fisheries Science Center’s West Coast Groundfish Bottom Trawl Survey: history, design, and description. DOI: 10.7289/V5/TM-NWFSC-136."))
                    ))
      )
}


test_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
    }
  )
}