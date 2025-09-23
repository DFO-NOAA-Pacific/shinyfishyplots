# shinyfishyplots

This repository contains a Shiny app for the Pacific Groundfish Data Synopsis. It relies on plotting functions and data from the [fishyplots](https://github.com/DFO-NOAA-Pacific/fishyplots) repository.

To build the Shiny app locally, first install these packages:

```R
install.packages("pak")
pak::pak("DFO-NOAA-Pacific/fishyplots")

install.packages("shiny")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("bslib")
install.packages("patchwork")
install.packages("shinycssloaders")
```

Then run the app:

```R
source("app.R")
shinyApp(ui = ui, server = server)
```
