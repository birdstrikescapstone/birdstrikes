pilotApp <- function() {
  tagList(
    setBackgroundColor(
      color = c("#F7FBFF", "#87CEFA"),
      gradient = "radial",
      direction = "bottom"
    ),
    div(
      class = "container",
      h1("App for Pilot and Air Crews", class = "title fit-h1"),
      p(""),
      p(""),
      
      fluidRow(tags$div(class = "container",
                        column(
                          6,
                          div(
                            class = "selectAirfield",
                            tags$style(
                              type = 'text/css',
                              ".selectize-input { font-size: 18px; line-height: 20px;} .selectize-dropdown { font-size: 18px; line-height: 20px; }"
                            ),
                            selectInput("airfield", tags$h4(tags$b(
                              "Select Airfield:"
                            )),
                            airfields),
                            style = "width: 60%; height: 200%",
                            class = "search"
                          )
                        ),
                        column(
                          6,
                          # tags$style('.input-sm {font-size: 14px; } label {font-weight: 500; margin-bottom: 15px; }'),
                          div(
                            class = "selectSearchAirfield",
                            airDatepickerInput(
                              "date",
                              tags$h4(tags$b("Select Date: ")),
                              value = as.character(Sys.Date()),
                              minDate = as.character(Sys.Date()),
                              maxDate = Sys.Date() + 150,
                              width = "12px"
                            ),
                            style = "width: 60%; height: 60%",
                            class = "search"
                          )
                        ))),
      
      fluidRow(tags$br()),
      
      fluidRow(
        tags$div(column(4,
                        tags$div(tags$h4(tags$b(
                          "Bird Strikes Risk Level:"
                        ))),
                        wellPanel(
                          tags$style(".well {background-color:Azure;}"),
                          tags$h4(
                            p(uiOutput("vboxrisk"), style = "font-size: 100%; color:blue;")
                          ))
                        ),
                 style = "width: 91%; margin-left: 121px;"),
        
        tags$div(column(4,
                        tags$div(tags$h4(tags$b(
                          "Historical Strike Count: "
                        )),
                        wellPanel(
                          tags$style(".well {background-color:Azure;}"),
                          tags$h4(
                            p(uiOutput("vboxstrikes"), style = "font-size: 150%; color:blue;")
                          ))
                        )),
                 style = "width: 91%; margin-left: 692px;"),
        
      ),
      
      fluidRow(tags$div(column(
        6,
        wellPanel(
          tags$h6("NOTE: <Something about the birds in the map and the risk level>")
        )
      ),
      style = "width: 200%;")),
      
      fluidRow(tags$br()),
      
      fluidRow(tags$br()),
      
      fluidRow(column(
        12,
        align = "center",
        div(
          width = 10,
          title = "Map of Airfield:",
          status = "primary",
          solidHeader = TRUE,
          collapsible = FALSE,
          height = "100%",
          leafletOutput(
            outputId = "map",
            width = "100%",
            height = 600
          )
        )
      ))
    )
  )
}


engineFailureApp <- function() {
  output.data <- readRDS("../data/engine_pred_data.RDS")
  numengs <- levels(output.data$numengs)
  season <- levels(output.data$season)
  
  tagList(
    div(
      class = "container",
      h1("App for Pilot and Air Crews", class = "title fit-h1"),
      h3("Predict Engine Failure", class = "title fit-h1"),
      p(""),
      p(""),
      
      fluidRow(tags$br()),
      
      fluidRow(tags$br()),
      
      fluidRow(tags$div(class = "container",
                        column(
                          6,
                          div(
                            class = "selectNumengs",
                            tags$style(
                              type = 'text/css',
                              ".selectize-input { font-size: 18px; line-height: 20px;} .selectize-dropdown { font-size: 18px; line-height: 20px; }"
                            ),
                            selectInput("numengs",
                                        tags$div(tags$h4(
                                          tags$b("Select Number of Engines:")
                                        )),
                                        choices = numengs),
                            style = "width: 60%; height: 200%; margin-left: 121px;",
                            class = "Engsearch"
                          )
                        ),
                        column(
                          6,
                          div(
                            class = "selectSeasons",
                            selectInput("season",
                                        tags$div(tags$h4(
                                          tags$b("Select Season:")
                                        )),
                                        choices = season),
                            style = "width: 60%; height: 60%;",
                            class = "Engsearch"
                          )
                        ))),
      
      fluidRow(tags$br()),
      
      fluidRow(
        tags$div(class = "container",
                 plotlyOutput("phPlot"),
                 style = "width: 100%; height: 400%;")
      ),
      
      fluidRow(tags$br()),
      
      fluidRow(tags$br()),
      
      fluidRow(
        tags$div(
          class = "container",
          plotlyOutput("skyPlot"),
          style = "width: 100%; height: 400%;"
        )
      )
    )
  )
}