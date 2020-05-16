source("Functions.R")

output.data <- readRDS("../data/engine_pred_data.RDS")

output.data$predicted <- round(output.data$predicted, 4)

function(input, output, session) {
  
  # Renders the map on the pilot app page
  
  output$map <- renderLeaflet({
    #get the coordinates of the input of the user
    coord <- air %>%
      filter(airfields == input$airfield)
    lat <- coord[1, 2]
    long <- coord[1, 3]
    
    #get the weather data on the map
    owm_data <-
      find_city(lat = coord[1, 2],
                lon = coord[1, 3] ,
                units = "imperial") %>%
      owmr_as_tibble()
    
    #get the bird data
    birds <- rebird::ebirdgeo(
      species = NULL,
      lat = coord[1, 2],
      lng = coord[1, 3],
      back = 1,
      dist = as.numeric(units::set_units(30, "mi")),
      key = EBIRD_KEY
    )
    #group the bird data
    birds <- birds %>% group_by(lat, lng) %>%
      summarise(howMany = sum(howMany))
    
    #map the data
    leaflet(birds) %>% addProviderTiles(providers$OpenStreetMap) %>%
      setView(zoom = 10.5,
              lat = coord[1, 2],
              lng = coord[1, 3]) %>%
      addProviderTiles(
        providers$OpenWeatherMap.Precipitation,
        options = providerTileOptions(apiKey = "99f521810d0fef37f59930f36dbb2256")
      ) %>%
      owmr::add_weather(owm_data,
                        template = "<b>{{name}}</b>, {{temp}}<c2><b0>F",
                        icon = owm_data$weather_icon) %>%
      addMarkers(
        lng = air$longitude,
        lat = air$latitude,
        popup = names(air)
      ) %>%
      addCircles(
        ~ as.numeric(air$longitude),
        ~ as.numeric(air$latitude),
        weight = 1,
        radius = ~ 10000
      ) %>%
      leaflet.extras::addHeatmap(
        lat = ~ birds$lat,
        lng = ~ birds$lng,
        blur = 20,
        max = 0.05,
        radius = 15
      )
  })
  
  
  # Renders the text required for the historical strikes
  
  output$vboxstrikes <- renderUI({
    t <- data %>%
      filter(`AIRPORT ID` == input$airfield) %>%
      group_by(`AIRPORT ID`) %>%
      summarise(STRIKES = sum(STRIKECOUNT)) %>%
      arrange(-STRIKES)
    
    tags$h2(tags$b(t[1, 2], style = "font-size: 100%; color:steelblue;"))
  })
  
  # Renders the text required for Bird strikes
  output$vboxrisk <- renderUI({
    risk <- getDataAndRunPredict(input$airfield, input$date)
    
    if (risk[1, 1] == "L") {
      tags$h2(tags$b("LOW RISK", style = "font-size: 100%; color:green;"))
    }
    else if (risk[1, 1] == "M") {
      tags$h2(tags$b("MEDIUM RISK", style = "font-size: 100%; color:Chocolate"))
    }
    else
      tags$h2(tags$b("HIGH RISK", style = "font-size: 100%; color:FireBrick"))
    })
    
    ###################################################################################################################
    
    # Engine Failure App Server Components
    
    ###################################################################################################################
  

  output$phPlot <- renderPlotly({
    analyze.phs <-
      output.data %>% dplyr::select(height, phase_of_flt, sky, eng_fail, predicted) %>%
      dplyr::filter((output.data$numengs == input$numengs) &
                      (output.data$height > 0 &
                         output.data$height < 10000) &
                      (output.data$season == input$season)
                    &
                      output.data$phase_of_flt %in% c(
                        "approach",
                        "arrival",
                        "climb",
                        "departure",
                        "descent",
                        "landingroll",
                        "takeoffrun"
                      )
      )
    
    ggplot(analyze.phs, aes(height, predicted, color = phase_of_flt)) +
      stat_smooth(
        method = "glm",
        family = binomial,
        formula = y ~ x,
        size = 1,
      ) +
      theme_bw() +
      theme(
        text = element_text(family = "Calibri", face = "bold"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(
          size = 12,
          face = "bold",
          angle = 0
        ),
        axis.text.y = element_text(
          size = 12,
          face = "bold",
          angle = 0
        ),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        panel.background = element_rect(fill = "transparent"),
        # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA),
        # bg of the plot
        legend.background = element_rect(fill = "transparent"),
        # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
      ) +
      scale_y_continuous(limits = c(0, 0.025)) +
      labs(
        title = "Probability of Engine Failure by Phase of Flight and Altitude",
        x = "Altitude",
        y = "Probability of Engine Failure",
        color = "Phase of Flight"
      )
    ggplotly()
  })
  
  output$skyPlot <- renderPlotly({
    analyze.phs <-
      output.data %>% dplyr::select(height, phase_of_flt, sky, eng_fail, predicted) %>%
      dplyr::filter((output.data$numengs == input$numengs) &
                      (output.data$height > 0 &
                         output.data$height < 10000) &
                      (output.data$season == input$season) &
                      (output.data$sky %in% c("nocloud", "overcast", "somecloud"))
      )
    
    ggplot(analyze.phs, aes(height, predicted, color = sky)) +
      stat_smooth(
        method = "glm",
        family = binomial,
        formula = y ~ x,
        size = 1,
      ) +
      theme_bw() +
      theme(
        text = element_text(family = "Calibri", face = "bold"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(
          size = 12,
          face = "bold",
          angle = 0
        ),
        axis.text.y = element_text(
          size = 12,
          face = "bold",
          angle = 0
        ),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        panel.background = element_rect(fill = "transparent"),
        # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA),
        # bg of the plot
        legend.background = element_rect(fill = "transparent"),
        # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
      ) +
      scale_y_continuous(limits = c(0, 0.025)) +
      labs(
        title = "Probability of Engine Failure by Sky Conditions and Altitude",
        x = "Altitude",
        y = "Probability of Engine Failure",
        color = "Sky Conditions"
      )
    ggplotly()
  })
  
  
  ###################################################################################################################
  
  # output$box_01 <- renderInfoBox({
  #   box1 <- valueBox(
  #     value = 20,
  #     icon = icon("users", lib = "font-awesome"),
  #     color = "blue",
  #     href = "https://wildlife.faa.gov/add",
  #     HTML(
  #       "<button id=\"button\" type=\"button\" class=\"btn btn-default action-button\">Report a Strike</button>"
  #     )
  #   )
  #   box1$children[[1]]$attribs$class<-"action-button"
  #   box1$children[[1]]$attribs$id<-"button_box_01"
  #
  #
  #   return(box1)
  # })
  # observeEvent(input$button_box_01, {
  #   toggleModal(session, "mod", "open")
  # })
  # output$tag <- renderUI({
  #   # urlfaa<- a("Click to Report a Strike", href="https://wildlife.faa.gov/add")
  #   tags$a(imageOutput("www/images/FAA.png"),
  #          href = "https://wildlife.faa.gov/add")
  #   #tagList(urlfaa)
  # })
  
  
  #
  # d <- reactive({
  #   dist <- switch(
  #     input$search,
  #     default="KDEN",
  #     kden = "KDEN",
  #     kdfw = "KDFW",
  #     kord = "KORD",
  #     ksmf = "KSMF")
  #   })
  
  # fieldLocation <- reactive({
  #   input$airfield
  # })
  
  # output$summary <- renderTable({
  #   getDataAndRunPredict(input$airfield,input$date)
  # })
  
  
  # output$test <- renderUI({
  #   tags$div(class = "container",
  #         tags$h3("Engine Failure:"),
  #         tags$p("TEXT", style = "font-size: 50%;"),
  #         # icon = shiny::icon("plane"),
  #         # width = 60,
  #         color = "orange")
  #  } )
  
  #   output$vboxengf <- renderUI({
  # tags$div(class = "box",
  #       tags$h3("Engine Failure:"),
  #       tags$p("TEXT", style = "font-size: 200%;"),
  #       icon = shiny::icon("plane"),
  #       # width = 60,
  #       color = "orange")
  #   })
  
  # output$gauge = renderGauge({
  #   risk <- getDataAndRunPredict(input$airfield, input$date)
  #   gauge(
  #     round(risk[1, 3], 1),
  #     min = 0,
  #     max = 1,
  #     label = "Risk",
  #     gaugeSectors(
  #       success = c(0, 0.99),
  #       warning = c(0, 0),
  #       danger = c(0, 0)
  #     )
  #   )
  #
  # })
  
  
  # output$vboxstrikes <- renderUI({
  #   t <- data %>%
  #     filter(`AIRPORT ID` == input$airfield) %>%
  #     group_by(`AIRPORT ID`) %>%
  #     summarise(STRIKES = sum(STRIKECOUNT)) %>%
  #     arrange(-STRIKES)
  #
  #   # tags$div(class = "content-box-blue",
  #
  #   tags$h2(tags$b(t[1, 2], style = "font-size: 100%; color:steelblue;"))
  #
  #
  #   # valueBox(
  #   #   tags$h5(t[1, 2], style = "font-size: 150%;"), icon = "fa-trash",
  #   #   color = ifelse("TEXT", "warning", "primary")
  #   # )
  #
  #   # fluidRow(
  #   # tags$div(
  #   # class = "box",
  #   # tags$div(
  #   # class = "content-box-blue",
  #   # icon = icon("earlybirds"),
  #   # tags$h5("Historical Strike Count: "),
  #   # tags$h5(t[1, 2], style = "font-size: 150%;")
  #   # style = "width: 200%; margin-left:370px"
  #   # )
  #
  # })
  
  
  
  # output$vboxrisk <- renderUI({
  #   risk <- getDataAndRunPredict(input$airfield, input$date)
  #
  #   if (risk[1, 1] == "L") {
  #     tags$h2(tags$b("LOW RISK", style = "font-size: 100%; color:green;"))
  #     # fluidRow(tags$div(
  #     #   class = "box",
  #     #   tags$div(
  #     #     class = "content-box-green",
  #     #     icon = icon("earlybirds"),
  #     #     tags$h5("Birdstrike Risk Level: "),
  #     #     tags$h5("LOW RISK", style = "font-size: 150%;"),
  #     #     style = "width: 200%;margin-left:90px"
  #     #   )
  #     # ))
  #   }
  #   else if (risk[1, 1] == "M") {
  #     tags$h2(tags$b("MEDIUM RISK", style = "font-size: 100%; color:Chocolate"))
  #     # fluidRow(tags$div(
  #     #   class = "box",
  #     #   tags$div(
  #     #     class = "content-box-orange",
  #     #     icon = icon("earlybirds"),
  #     #     tags$h5("Birdstrike Risk Level: "),
  #     #     tags$p("MEDIUM RISK", style = "font-size: 150%;"),
  #     #     style = "width: 200%;margin-left:90px"
  #     #   )
  #     # ))
  #   }
  #   else
  #     tags$h2(tags$b("HIGH RISK", style = "font-size: 100%; color:FireBrick"))
  #   # fluidRow(tags$div(
  #   #   class = "box",
  #   #   tags$div(
  #   #     class = "content-box-red",
  #   #     icon = icon("earlybirds"),
  #   #     tags$h5("Birdstrike Risk Level: "),
  #   #     tags$p("HIGH RISK", style = "font-size: 150%;"),
  #   #     style = "width: 200%;margin-left:90px"
  #   #   )
  #   # ))
  #   # ))
  # })
  
  
}



