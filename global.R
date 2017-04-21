## GLOBAL --------------------------------------------------------------------
library(dplyr)
library(shiny)
library(highlight)
library(ggplot2)
library(Luminescence)
sapply(list.files("snippets/", "\\.R$", full.names = TRUE), source)

## Header
about_text <- paste("This presentation was entirely written in <b>R</b> using the",
                    tags$code("shiny"), "framework. Everything you see in this presentation",
                    "is dynamically rendered by <b>R</b> or, more specifically, by",
                    tags$code("Shiny Server"), "running on a server hosted at the Institute of Geography of",
                    "the University of Cologne.", tags$br(), tags$br(),
                    "If you want to have a look at, download or re-use the <b>R</b> code",
                    "used to produce this web application please see the 'Get started' section",
                    "on the side bar and scan the corresponding QR code. Note that for this PICO presentation",
                    "hyperlinks were deliberately removed to prevent undesired context switches",
                    "and thus to retain its integrity during the conference. If you are watching this presentation",
                    "on a mobile device please open the following URL in a desktop browser", tags$br(), tags$br(),
                    tags$a(href = "#", "http://rlum.geographie.uni-koeln.de:3838/EGU2017-PICO/"), tags$br(), tags$br(),
                    "in order to be able to scan the QR codes. If you want to contact the author",
                    "of this presentation you can send an email to", tags$a(href = "#", "christoph.burow@uni-koeln.de"),
                    ".", tags$br(), tags$br(),
                    actionButton("dummyBtn", "", icon = icon("info-circle"), class = "info-btn"),
                    "Watch out for these buttons for additional text information!") %>% 
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

## Solution text
solution_text <- paste(
"
In Human-Computer Interaction an alternative approach to the
command-line interface (<b>CLI</b>) is the graphical user interface
(<b>GUI</b>), which allows direct, interactive manipulation and
interaction with the underlying software. For users with
little or no experience with command-lines a GUI offers
intuitive access that counteracts the perceived steep learning
curve of a CLI (Unwin & Hofmann, 1999).", tags$br(), tags$br(),
"Here, we present a GUI for the R package <code>Luminescence</code>
in the form of interactive web applications. These applications
can be accessed online so that a user is not even
required to have a local installation of <b>R</b>. The so-called shiny
applications provide access to most of the plotting functions
of the R package <code>Luminescence</code> as well as to the functions
for calculating the cosmic dose rate and for transforming
CW-OSL curves. We further introduce the <b>R</b>
package <code>RLumShiny</code> (Burow, 2016) that bundles all
applications, is freely available through the CRAN and
GitHub, and which can be installed and used in any local
<b>R</b> environment.
"
) %>% 
  HTML()

hello_shiny_text <- paste(
  "
  A shiny application consists of the two files named <code>Ui.R</code> and <code>Server.R</code>.
  The <code>Ui.R</code> file is used for the user-interface definition, i.e. where 
  you design the look and feel of your graphical user interface and define all the input elements the
  user can interact with. The code in <code>Ui.R</code> is used to generate and render a HTML page like the one
  you are looking at right at this moment. Without going too deep into the details about the various options when
  designing the page layout itself, the most straight-forward layout is produced by the function
  <code>fluidPage()</code> and the nested functions <code>titlePanel()</code>, 
  <code>sidebarLayout()</code> and <code>mainPanel()</code>. Using this layout the input elements 
  (so-called 'widgets') are placed in the sidebar, and output elements are defined in the main panel.
  In the example shown here the application consists of only one input element (<code>sliderInput()</code>)
  and a panel to show a plot (<code>plotOutput()</code>).", tags$br(), tags$br(),
  "The <code>Server.R</code> defines the actual server logic. Think of this file more of a normal
  <b>R</b> script file you would write to e.g. produce a plot, but instead of using static 
  variables you have access to the output of the input widgets defined in the <code>Ui.R</code>.
  Naturally, as the user interacts with the input widget its return value changes, and depending
  the calculations defined in the <code>Server.R</code> the output also changes. In our example
  the user interacts with a simple slider that returns a <code>numeric</code> value, which
  in turn is used in the <code>Server.R</code> (<code>input$bins</code>) to plot a histogram
  with the specified number of bins.", tags$br(), tags$br(),
  "The real 'magic' of shiny is that any changes to the input widgets are automatically recognized,
  which then triggers the <code>Server.R</code> to re-run all calculations necessary to update
  the output (e.g. the plot)."
) %>% 
  HTML()

deploy_text <- paste(
"There is basically two different ways of sharing and running a shiny
application: (i) on a local computer with a working installation of 
<b>R</b> and RStudio, or (ii) by serving the applications as a web-service,
where the user is only required to have a HTML5 compliant web browser and
an internet connection.", tags$br(), tags$br(),
"In the case of the here presented shiny applications for the <b>R</b> package
<code>Luminescence</code> they are distributed as a separate <b>R</b> package
named <code>RLumShiny</code>. To run the applications included in this package 
the user is only required to install the package once via <code>install.packages('RLumShiny')</code>
and then call the function <code>app_RLum()</code>", tags$br(), tags$br(),
"An alternative to installing and using the 'RLumShiny'
package on a local computer is to host the applications as a
web service using the <code>Shiny Server</code> software. This enables
sharing the applications with a wider user base, whether it
be an organisation, a working group or anyone interested in
using it by making it freely accessible on the internet. Some
of the advantages include that, amongst all potential users of
the service, only one person is required to set up and maintain
the Shiny Server. It has to be considered, however, that
setting up a Shiny Server requires a server (or web space),
which may need to be purchased or rented first, and a person
with sufficient knowledge in administrating a Linux server.
Furthermore, the open source version of Shiny Server only
has a limited amount of features compared to the Pro version
that is subject to fee. Nonetheless, the advantages of running
a freely accessible, local or access limited Shiny Server can
far outweigh these drawbacks and once set up, can provide
unlimited and platform independent access to the shiny applications.",
tags$br(), tags$br(),
"In some cases it may also be viable to use RStudio's self-service platform
<a href = '#'>http://shinyapps.io</a>, a hosting environment where
users can easily upload, run and share their shiny applications.
The service offers different subscription plans depending
on the desired number of allowed applications, service
availability and feature content.
"
) %>% 
  HTML()

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
  
  cosmic = tags$iframe(seamless = "seamless", 
                     src = "http://rlum.geographie.uni-koeln.de:3838/packages/RLumShiny/inst/shiny/cosmicdose/", 
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

## RLumShiny
rlumshiny_app_df <- data.frame(
  Application = c("Abanico Plot", 
                  "Radial Plot", 
                  "Histogram", 
                  "Kernel Density Estimate Plot", 
                  "Dose Recovery Test", 
                  "Cosmic Dose Rate",
                  "CW Curve Transformation"),
  Function = c("plot_AbanicoPlot()",
               "plot_RadialPlot()",
               "plot_Histogram()",
               "plot_KDE()",
               "plot_DRTResults()",
               "calc_CosmicDoseRate()",
               "CW2pHMi(), CW2pLM(), CW2pLMi(), CW2pPMi()")
)

rlumshiny_fun_df <- data.frame(
  Function = c("app_RLum()", "jscolorInput()", "popover()", "tooltip()"),
  Description = c("Run luminescence shiny applications", 
                  "Creates a JSColor widget to be used in shiny applications.",
                  "Create a bootstrap button with popover.",
                  "Create bootstrap tooltips for any HTML element to be used in shiny applications.")
)