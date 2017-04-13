# Dependencies
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
source("global.R")


################################################################################
####                          SERVER                                          ##
################################################################################
ui <- dashboardPage(
  
  ## ------------------------------------------------------------------------ ##
  ##                             HEADER
  ## ------------------------------------------------------------------------ ##
  dashboardHeader(title = "EGU 2017 - PICO",
                  tags$li(class = "dropdown", 
                          tags$a(id = "btnFullscreen", icon("arrows-alt"), "Toggle Fullscreen")),
                  tags$li(class = "dropdown", 
                          tags$a(href = "", icon("refresh"), "Reload"))
                  
  ),## EndOf::HEADER
  
  ## ------------------------------------------------------------------------ ##
  ##                             SIDEBAR
  ## ------------------------------------------------------------------------ ##
  dashboardSidebar(
    
    # Talk title
    tags$p(id = "talk-title", align = "center",
           HTML("Easing access to R using ‘shiny’ to create graphical user interfaces:</br>",
                "An example for the R package ‘Luminescence’")),
    
    # Sidebar navigation items
    sidebarMenu(id = "sidebar",
                
                # Menus
                menuItem("Introduction", icon = icon("play"), tabName = "intro",
                         menuSubItem(HTML("The p<b>R</b>oblem"), icon = icon("question-circle-o"), tabName = "intro_1"),
                         menuSubItem("The Solution(?)", icon = icon("lightbulb-o"), tabName = "intro_2")),
                
                menuItem("The 'shiny' framework", icon = icon("star"), tabName = "shiny",
                         menuSubItem("Hello Shiny!", icon = icon(""), tabName = "shiny_1"),
                         menuSubItem("Widgets", icon = icon(""), tabName = "shiny_2"),
                         menuSubItem("Deploying apps", icon = icon(""), tabName = "shiny_3")),
                
                menuItem("From CLI...", icon = icon("terminal"), tabName = "lum",
                         menuSubItem("'Luminescence' package", icon = icon(""), tabName = "lum_1"),
                         menuSubItem("How it all started", icon = icon(""), tabName = "lum_2"),
                         menuSubItem("Current content", icon = icon(""), tabName = "lum_3"),
                         menuSubItem("Reception", icon = icon(""), tabName = "lum_4")),
                
                menuItem("...to GUI", icon = icon("television"), tabName = "shinylum",
                         menuSubItem("'RLumShiny' package", icon = icon(""), tabName = "shinylum_1"),
                         menuSubItem("Examples", icon = icon(""), tabName = "shinylum_2")),
                
                menuItem("Authors", icon = icon("user"), tabName = "authors")
    )#EndOf::SideBarMenu
    
  ),#EndOf::SIDEBAR
  
  ## ------------------------------------------------------------------------ ##
  ##                             BODY
  ## ------------------------------------------------------------------------ ##
  dashboardBody(
    
    ## -----
    ## Include (javascript, css)
    tags$header(
      tags$script(src = "js/fullscreen.js"),
      tags$script(src = "js/highlight.pack.js"),
      tags$script(src = "js/shiny-showcase.js"),
      tags$link(rel = "stylesheet", type = "text/css", href = "js/styles/dracula.css"),
      tags$script("hljs.initHighlightingOnLoad();"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css")
    ),
    
    tabItems(
      ## Introduction ----
      tabItem("intro_1",
              fluidRow(
                box(width = 8, status = "primary", solidHeader = TRUE,
                    title = "Introduction #1"),
                box(width = 4, status = "warning", 
                    title = "Additional Information")
              ),
              fluidRow(
                box(width = 6, status = "primary", solidHeader = FALSE,
                    title = "Expectation",
                    tags$img(src = "img/expectation.gif", style = "width:100%;", border = 0, alt = "Null")),
                box(width = 6, status = "warning", solidHeader = FALSE,
                    title = "Reality",
                    tags$img(src = "img/reality.gif", style = "width:100%;", border = 0, alt = "Null"))
              )
      ),
      tabItem("intro_2",
              fluidRow(
                box(width = 8, status = "primary", solidHeader = TRUE,
                    title = "Introduction #2")
              )),
      
      ## Shiny framework ----
      # Hello Shiny app
      tabItem("shiny_1",
              fluidRow(
                tabBox(title = "Code",
                       width = 6,
                       tabPanel("Ui.R", htmlOutput("helloshiny_code_ui")),
                       tabPanel("Server.R", htmlOutput("helloshiny_code_server"))
                ),
                box(width = 6, status = "primary", solidHeader = TRUE,
                    title = "Hello Shiny!",
                    plotOutput("helloshiny"),
                    sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30)
                )
              )
      ),
      # Input widgets
      tabItem("shiny_2",
              fluidRow(
                column(width = 6,
                       fluidRow(width = 12,
                                box(title = "Action button",
                                    status = "primary",
                                    solidHeader = TRUE, collapsible = TRUE,
                                    width = 6,
                                    actionButton("actionBtn", "Do NOT press!", class = "btn btn-danger", style = "color: white;")
                                ),
                                box(title = "Checkbox",
                                    status = "primary",
                                    solidHeader = TRUE, collapsible = TRUE,
                                    width = 6,
                                    checkboxInput("checkbox", "Check this out!", value = FALSE)
                                )
                       ),
                       fluidRow(width = 12,
                                box(title = "Checkbox group", 
                                    status = "primary",
                                    solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                    width = 6,
                                    checkboxGroupInput("checkboxGroup", "Check us out!", 
                                                       selected = c("Blue", "Green", "Red"),
                                                       choices = list("Choice 1" = "Blue", 
                                                                      "Choice 2" = "Green",
                                                                      "Choice 3" = "Red"))
                                ),
                                box(title = "Date input",
                                    status = "primary",
                                    solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                    width = 6,
                                    dateInput("dateInput", "Date input"))
                       ),
                       fluidRow(width = 12,
                                box(title = "Date range", 
                                    status = "primary",
                                    solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                    width = 6,
                                    dateRangeInput("dateRange", "Date range") 
                                ),
                                box(title = "File input",
                                    status = "primary",
                                    solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                    width = 6,
                                    fileInput("fileInpute", "File input"))
                       ),
                       fluidRow(width = 12,
                                box(title = "Numeric input", 
                                    status = "primary",
                                    solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                    width = 6,
                                    numericInput("numericInput", "Numeric input", value = 42) 
                                ),
                                box(title = "Radio buttons",
                                    status = "primary",
                                    solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                    width = 6,
                                    radioButtons("radioButtons", "Radio buttons", 
                                                 choices = list("Choice 1" = 1,
                                                                "Choice 2" = 2,
                                                                "Choice 3" = 3)))
                       ),
                       fluidRow(width = 12,
                                box(title = "Select box", 
                                    status = "primary",
                                    solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                    width = 6,
                                    selectInput("selectBox", "Select box", 
                                                choices = list("Choice 1" = 16,
                                                               "Choice 2" = 1)) 
                                ),
                                box(title = "Slider",
                                    status = "primary",
                                    solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                    width = 6,
                                    sliderInput("slider", "Slider", 
                                                min = 1, max = 10, value = 2, step = 1))
                       ),
                       fluidRow(width = 12,
                                box(title = "Slider range", 
                                    status = "primary",
                                    solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                    width = 6,
                                    sliderInput("sliderRange", "Slider range", 
                                                min = 0, max = 100, value = c(0, 100), step = 5)
                                ),
                                box(title = "Text input",
                                    status = "primary",
                                    solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                    width = 6,
                                    textInput("textInput", "Text input", placeholder = "Enter text...")
                                )
                       )
                ),
                column(width = 6,
                       box(title = "Output",
                           status = "warning",
                           solidHeader = TRUE,
                           width = 12,
                           plotOutput("widget_plot"))
                )
              )
      ),
      
      ## Luminescence ----
      tabItem("lum_3",
              box(width = 12, status = "primary", solidHeader = FALSE,
                  title = HTML("Functions in the <b>R</b> package 'Luminescence' (v0.8.0)"),
                  dataTableOutput("lum_functions"))
      ),
      
      ## RLumShiny ----
      tabItem("shinylum_1",
              tabBox(title = HTML("The <b>R</b> package 'RLumShiny'"),
                     width = 12,
                     tabPanel("tab 1"),
                     tabPanel("tab 2")
              )
      ),
      tabItem("shinylum_2",
              tabBox(title = "RLumShiny applications",
                     id = "examples_1",
                     width = 12,
                     tabPanel("Abanico Plot", uiOutput("abanico")),
                     tabPanel("KDE", uiOutput("kde")),
                     tabPanel("Histogram", uiOutput("hist"))
              )
      )
    )
  )#EndOf::BODY
)

################################################################################
####                          SERVER                                          ##
################################################################################
server <- function(input, output, session) {
  
  ## Introduction
  
  ## Shiny framework
  output$helloshiny_code_ui <- renderUI({
    helloshiny_code$ui
  })
  output$helloshiny_code_server <- renderUI({
    helloshiny_code$server
  })
  
  output$helloshiny <- renderPlot({
    x <- faithful[ ,2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = "darkgray", border = "white")
  })
  
  ## Shiny widget plot
  output$widget_plot <- renderPlot({
    
    if (is.null(input$checkboxGroup))
      return(NULL)
    
    n <- input$numericInput^1.5
    
    df <- data.frame(x = runif(n = n, 0, 100),
                     y = runif(n = n, 0, 100),
                     color = sample(c("Blue", "Green", "Red"), size = n, replace = TRUE)) %>% 
      filter(color %in% input$checkboxGroup)
    
    gg <- ggplot(df, aes(x, y, color = color)) +
      geom_point(size = input$slider, shape = as.integer(input$selectBox)) +
      xlim(input$sliderRange) + 
      ylim(input$sliderRange) +
      xlab("A random number") +
      ylab("Another random number") +
      ggtheme[[as.integer(input$radioButtons)]] +
      ggtitle(get_buttonMsg(input$actionBtn))
    
    if (input$checkbox)
      gg <- gg + facet_grid(. ~ color)
    
    gg
  })
  
  ## Luminescence functions table
  output$lum_functions <- renderDataTable({
    rlum_fun_df
  }, options = list(pageLength = 10, pagingType = "full", 
                    searching = FALSE, lengthChange = FALSE))
  
  ## RLumShiny example applications
  output$abanico <- renderUI({
    iframe$abanico
  })
  output$kde <- renderUI({
    iframe$kde
  })
  output$hist <- renderUI({
    iframe$hist
  })
  
}


# ----
shinyApp(ui, server)