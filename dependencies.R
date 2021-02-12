list.of.packages <- c("shiny", "shinydashboard","treemap","wordcloud2", "dplyr", "leaflet", 
                      "DT", "readxl", "writexl", "GADMTools","tm","SnowballC", "RColorBrewer", 
                      "ggplot2","shinyBS", "plotly","shinyWidgets")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, library, character.only = TRUE)