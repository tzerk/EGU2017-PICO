## GLOBAL --------------------------------------------------------------------
library(dplyr)
library(shiny)
library(highlight)
library(ggplot2)
library(Luminescence)
source("snippets/package_stats.R")
source("snippets/solution_plot.R")

## Header
about_text <- paste("This presentation was entirely written in <b>R</b> using the",
                    tags$code("shiny"), "framework. Everything you see in this presentation",
                    "is dynamically rendered by <b>R</b> or, more specifically, by",
                    tags$code("Shiny Server"), "running on a server hosted at the Institute of Geography at",
                    "the University of Cologne.", tags$br(), tags$br(),
                    "If you want to have a look at, download or re-use the <b>R</b> code",
                    "used to produce this web application please see the 'Further resources' section",
                    "on the side bar and scan the corresponding QR code. Note that for this PICO presentation",
                    "hyperlinks were deliberately removed to prevent undesired context switches",
                    "and thus to retain its integrity during the conference. If you are watching this presentation",
                    "on a mobile device please open the following URL in a desktop browser", tags$br(), tags$br(),
                    tags$a(href = "#", "http://rlum.geographie.uni-koeln.de:3838/EGU2017-PICO/"), tags$br(), tags$br(),
                    "in order to be able to scan the QR codes. If you want to contact the author",
                    "of this presentation you can send an email to", tags$a(href = "#", "christoph.burow@uni-koeln.de"),
                    ".") %>% 
  HTML()

solution_text <- paste("
                       placeholder
                       ") %>% 
  HTML()

## Presentation abstract
title <- HTML(paste("Easing access to R using 'shiny' to create graphical user interfaces:<br/>",
                    " An example for the R package 'Luminescence'"))

abstract <- as.list(strsplit(paste(readLines("snippets/abstract.txt", warn = FALSE), collapse = ""), "<br/>")[[1]])

authors <- HTML(paste("Christoph Burow[1], Sebastian Kreutzer[2], Michael Dietze[3], Margret C. Fuchs[4],",
                      "Christoph Schmidt[5], Manfred Fischer[5], Helmut Br&uuml;ckner[1]"))

affils <- list("University of Cologne, Institute of Geography, Department of Geosciences, Cologne, Germany (christoph.burow@uni-koeln.de)",
               "IRAMAT-CRP2A, Universit&eacute; Bordeaux Montaigne, Maison d'Arch&eacute;ologie, Esplanades des Antilles, 33607 Pessac Cedex, France",
               "Section 5.1: Geomorphology, Helmholtz Centre Potsdam, GFZ German Research Centre for Geosciences, Potsdam, Germany",
               "Helmholtz-Zentrum Dresden-Rossendorf, Helmholtz-Institut Freiberg for Resource Technology, Freiberg, Germany",
               "Chair of Geomorphology, University of Bayreuth, Bayreuth, Germany")

## Problem description
problem_text <- paste(
"While <b>R</b> is a comparatively easy-to-learn programming
language, there is still a steep learning curve until a user
is able to routinely achieve the desired results. In-depth
knowledge of <b>R</b> fundamentals is not required when
working with the 'Luminescence' package, but being
familiar with the most important data structures in R is
a must.", tags$br(), tags$br(),
"In the simplest case, for a specific task, using
the package only involves a single short function call,
e.g., ", tags$code("Luminescence::plot AbanicoPlot(data = de.data)"), " to produce
an abanico plot (Dietze et al., 2016)
of equivalent dose estimates. However, users may want to
adjust the plot according to their requirements. While other
software products such as Origin&reg; or SigmaPlot&reg; allow the
user to comfortably click on each element of a plot to change
its appearance, this is not possible in <b>R</b>. In <b>R</b> a plot cannot
be changed after it has been drawn, and the user is required
to re-run the function call with additional arguments that
control the appearance of specific plot elements. For the",
tags$code("Luminescence::plot AbanicoPlot()"), "function
there are currently 33 such arguments, plus additional base
<b>R</b> arguments that can be used to design the plot to ones
desire. For more elaborate plots the function call in the <b>R</b>
command-line rapidly increases in complexity.", tags$br(), tags$br(),
"Users new to <b>R</b> may feel quickly overwhelmed and may hence not be able
to exploit the full potential of the <b>R</b> command-line. But even
experienced users may find it tedious to iteratively run the
function until a satisfying results is produced."
) %>% HTML()

## Solution data
data <- readRDS("data/solution_DeValues.rds")

## Luminescence functions table
rlum_fun_df <- readRDS("data/rlum_functions.rds") %>% 
  select(Name, Title, Version)

## RLumShiny iframe links
iframe <- list(
  abanico = tags$iframe(seamless = "seamless", 
                        src = "http://rlum.geographie.uni-koeln.de:3838/packages/RLumShiny/inst/shiny/abanico/", 
                        width = "100%", 
                        height = "650px",
                        frameborder = "0"),
  
  kde = tags$iframe(seamless = "seamless", 
                    src = "http://rlum.geographie.uni-koeln.de:3838/packages/RLumShiny/inst/shiny/KDE/", 
                    width = "100%", 
                    height = "650px",
                    frameborder = "0"),
  
  hist = tags$iframe(seamless = "seamless", 
                     src = "http://rlum.geographie.uni-koeln.de:3838/packages/RLumShiny/inst/shiny/histogram/", 
                     width = "100%", 
                     height = "650px",
                     frameborder = "0")
)

highlight_code <- function(file) {
  x <- highlight(file, renderer = renderer_html(), output = NULL) %>% 
    paste(collapse = "") %>%
    HTML()
  invisible(x)
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