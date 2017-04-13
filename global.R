## GLOBAL --------------------------------------------------------------------
library(dplyr)
library(shiny)
library(highlight)
library(ggplot2)

## Presentation abstract
title <- HTML(paste("Easing access to R using 'shiny' to create graphical user interfaces:<br/>",
                    " An example for the R package 'Luminescence'"))

abstract <- as.list(strsplit(paste(readLines("snippets/abstract.txt"), collapse = ""), "<br/>")[[1]])

authors <- HTML(paste("Christoph Burow[1], Sebastian Kreutzer[2], Michael Dietze[3], Margret C. Fuchs[4],",
                      "Christoph Schmidt[5], Manfred Fischer[5], Helmut Br&uuml;ckner[1]"))

affils <- list("University of Cologne, Institute of Geography, Department of Geosciences, Cologne, Germany (christoph.burow@uni-koeln.de)",
               "IRAMAT-CRP2A, Universit&eacute; Bordeaux Montaigne, Maison d'Arch&eacute;ologie, Esplanades des Antilles, 33607 Pessac Cedex, France",
               "Section 5.1: Geomorphology, Helmholtz Centre Potsdam, GFZ German Research Centre for Geosciences, Potsdam, Germany",
               "Helmholtz-Zentrum Dresden-Rossendorf, Helmholtz-Institut Freiberg for Resource Technology, Freiberg, Germany",
               "Chair of Geomorphology, University of Bayreuth, Bayreuth, Germany")
  
## Luminescence functions table
rlum_fun_df <- readRDS("data/rlum_functions.rds") %>% 
  select(Name, Title, Version)

## RLumShiny iframe links
iframe <- list(
  abanico = tags$iframe(seamless = "seamless", 
                        src = "http://rlum.geographie.uni-koeln.de:3838/packages/RLumShiny/inst/shiny/abanico/", 
                        width = "100%", 
                        height = "650px"),
  
  kde = tags$iframe(seamless = "seamless", 
                    src = "http://rlum.geographie.uni-koeln.de:3838/packages/RLumShiny/inst/shiny/KDE/", 
                    width = "100%", 
                    height = "650px"),
  
  hist = tags$iframe(seamless = "seamless", 
                     src = "http://rlum.geographie.uni-koeln.de:3838/packages/RLumShiny/inst/shiny/histogram/", 
                     width = "100%", 
                     height = "650px")
)

highlight_code <- function(file) {
  highlight(file, renderer = renderer_html()) %>% 
    paste(collapse = "") %>%
    HTML()
}

## shiny framework
helloshiny_code <- list(ui = highlight_code("snippets/hello_shiny_ui.R"),
                        server = highlight_code("snippets/hello_shiny_server.R")
                        )

## Input button messages
buttonMsgs <- c("I said: do NOT push",
                "Really?", "Reeeaaally?",
                "WARNING: Scientist detected!",
                "Please stop...", "Stop it!", "STOP",
                "Aaahhhh", "You really are a scientist, are you?",
                "Ok, I give up...")

get_buttonMsg <- function(x) {
  if (x == 0)
    return("A fancy plot")

  n <- as.character(x)
  buttonMsgs[as.integer(substr(n, nchar(n), nchar(n)))]
}

# gg plot themes
ggtheme <- list(theme_bw(), theme_classic(), theme_dark())