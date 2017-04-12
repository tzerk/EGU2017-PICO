library(shiny)
library(shinydashboard)
source("global.R")

ui <- dashboardPage(
  
  ## ------------------------------------------------------------------------ ##
  ##                             HEADER
  ## ------------------------------------------------------------------------ ##
  dashboardHeader(title = "EGU 2017",
                  tags$li(class = "dropdown", tags$a(id = "btnFullscreen", icon("arrows-alt"), "Toggle Fullscreen")),
                  tags$li(class = "dropdown", tags$a(href = "", icon("refresh"), " Reload"))
                  
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
                menuItem("Introduction", icon = icon("play"), tabName = "intro", selected = TRUE,
                         menuSubItem(HTML("The p<b>R</b>oblem"), icon = icon("question-circle-o"), tabName = "intro_1", selected = TRUE),
                         menuSubItem("The Solution(?)", icon = icon("lightbulb-o"), tabName = "intro_2")),
                
                menuItem("The 'shiny' framework", icon = icon("star"), tabName = "shiny",
                         menuSubItem("Hello Shiny!", icon = icon(""), tabName = "shiny_1"),
                         menuSubItem("Widgets", icon = icon(""), tabName = "shiny_2"),
                         menuSubItem("Deploying apps", icon = icon(""), tabName = "shiny_3")),
                
                menuItem("The 'Luminescence' package", icon = icon("archive"), tabName = "lum",
                         menuSubItem("How it all started", icon = icon(""), tabName = "lum_1"),
                         menuSubItem("Current status", icon = icon(""), tabName = "lum_2"),
                         menuSubItem("Reception", icon = icon(""), tabName = "lum_3")),
                
                menuItem("The 'RLumShiny' package", icon = icon("archive"), tabName = "shinylum",
                         menuSubItem("Examples", icon = icon(""), tabName = "shinylum_1")),
                
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
      tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css")
    ),
    
    
    tags$iframe(seamless = "seamless", src = "http://rlum.geographie.uni-koeln.de:3838/packages/RLumShiny/inst/shiny/abanico/", width = "100%", height = "1000px")
    
  )
)

server <- function(input, output) { }

shinyApp(ui, server)