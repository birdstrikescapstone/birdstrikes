#  #Requiring all of the predefined libraries and functions
source("appParts.R") 
source("Functions.R")
 

# #---------------------------- OTHER UI -----------------------------
shinyUI(
  navbarPage(
    title = tags$b("Bird Strikes"),
    theme = "style/style.css",
    fluid = TRUE,
    collapsible = TRUE,
    # tab panel 1 - Home
    tabPanel(
      tags$b("Home"),
      includeHTML("home.html"),
      tags$script(src = "plugins/scripts.js"),
      tags$head(tags$link(rel = "stylesheet",
                          type = "text/css"))
    ),
    # tab panel 2 - Neighborhood Browser
    tabPanel(tags$b("Pilot App"),
             pilotApp()),
    
    # tab panel 3 - Location Comparison
    tabPanel(tags$b("Engine Failure Analysis"),
             engineFailureApp()),
    
    # tab panel 4 - About
    tabPanel(
      tags$b("About Team"),
      includeHTML("about.html"),
      shinyjs::useShinyjs(),
      tags$head(
        tags$link(rel = "stylesheet",
                  type = "text/css",
                  href = "plugins/carousel.css"),
        tags$script(src = "plugins/holder.js")
      ),
      tags$style(
        type = "text/css",
        ".shiny-output-error { visibility: hidden; }",
        ".shiny-output-error:before { visibility: hidden; }"
      )
    )
    
  )
)



##########################################################################################################

# Backup

# shinyUI(navbarPage(title = tags$b("Bird Strikes"),
#                    theme = "style/style.css",
#                    # footer = includeHTML("footer.html"),
#                    fluid = FALSE,
#                    collapsible = TRUE,
#                    
#                    # tags$head(tags$style(HTML('/* body */.content-wrapper, .right-side {background-color: #7d848b;}'))),
#                    # tab panel 1 - Home
#                    tabPanel(tags$b("Home"),
#                             includeHTML("home.html"),
#                             tags$script(src = "plugins/scripts.js"),
#                             tags$head(
#                               tags$link(rel = "stylesheet",
#                                         type = "text/css"),
#                               
#                               # href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"),
#                               # tags$link(rel = "icon",
#                               #           type = "image/png",
#                               #           href = "images/logo_icon.png")
#                             )
#                    ),
#                    # tab panel 2 - Neighborhood Browser
#                    tabPanel(tags$b("Pilot App"),
#                             # tags$head(tags$style(HTML(mycss))),
#                             pilotApp(),
#                             # includeHTML("scrollToTop.html"),
#                             # tags$script(src = "plugins/scripts.js"),
#                             # tags$style(HTML(".box-header{background:#d2d2d2; color:#d83000; text-align:center;}"))
#                    ),
#                    
#                    # tab panel 3 - Location Comparison
#                    tabPanel(tags$b("Engine Failure Analysis"),
#                             "propertyComparison()"
#                    ),
#                    
#                    # tab panel 4 - About
#                    tabPanel(tags$b("About Team"),
#                             includeHTML("about.html"),
#                             shinyjs::useShinyjs(),
#                             tags$head(
#                               tags$link(rel = "stylesheet",
#                                         type = "text/css",
#                                         href = "plugins/carousel.css"),
#                               tags$script(src = "plugins/holder.js")
#                             ),
#                             tags$style(type="text/css",
#                                        ".shiny-output-error { visibility: hidden; }",
#                                        ".shiny-output-error:before { visibility: hidden; }"
#                             )
#                    )
#                    
# ))


#---------------------------- Pilot App ----------------------------- 
#R User Interface
# dashboardPage(skin = "blue",
#               dashboardHeader(title = "Birdstrikes"),
#               
# 
#               #Sidebar for inputs
#               dashboardSidebar(
#                 sidebarMenu(
#                   menuItem("Pilot Dashboard", tabName = "Inputs", icon = icon("dashboard")),
#                   selectInput("airfield","Airfields:",
#                               airfields),
# 
#                   dateInput("date","Date:",
#                             value = as.character(Sys.Date()),
#                             min = as.character(Sys.Date()),
#                             max = Sys.Date()+500),
# 
#                   menuItem("About WTF!", tabName = "Inputs", icon = icon("users"))
# 
#                 )
#               ),
#               #Body
#               dashboardBody(
#                 fluidRow(),
#                   
#                 fluidRow(
#                   infoBoxOutput("vboxrisk"),
#                   valueBoxOutput("vboxstrikes"),
#                   valueBoxOutput('vboxengf')),
#                     # actionButton(inputId='vboxengf', label="Learn More",
#                     #                           icon = icon("bird"), 
#                     #                           onclick ="window.open('https://wildlife.faa.gov/add')"))
#                   
#                 
#                 fluidRow(
#                   # box(tags$style(type = "text/css", "html, body {width:100%;height:200%}"),
#                   #            leaflet::leafletOutput("map"),
#                   #            absolutePanel(id="controls",
#                   #                          style="z-index:500;",
#                   #                          class = "panel panel-default",
#                   #                          draggable = TRUE),
#                   #            width = 12)
#                   
#                   column(12, align = "center",
#                     box(
#                       width = 10,
#                       title = "Map of Airfield:",
#                       status = "primary",
#                       solidHeader = TRUE,
#                       collapsible = FALSE,
#                       height = "100%",
#                       leafletOutput(outputId = "map", width="100%", height = 940))),
#                   box(uiOutput("tag"),
#                       width = 4,
#                       height = 3),
#                   valueBoxOutput("box_01"),
#                   bsModal("mod","Report a Strike","btn")
#                   
#                   ),
#                 
#                 
#                 # fluidRow(
#                 #   box(tableOutput("summary"),
#                 #   width = 4,
#                 #   background = "light-blue",
#                 #   p("This is content. The background color is set to light-blue")))
#                 # ,fluidRow(
#                 #   align="LEFT", tags$img(src='images/1 Logo.png', width = "300px")
#                 # ),
# 
#               )
# 
# )

##hyperlink the faa strike data base as an option 