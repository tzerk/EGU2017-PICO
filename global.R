## GLOBAL --------------------------------------------------------------------
library(dplyr)
library(shiny)
library(highlight)
library(ggplot2)

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