
# Loading Libraries ############################################################
library(shiny)
library(shinythemes)
library(bslib)
library(tidyverse)
library(plotly)
library(here)
library(giscoR)
library(scales)
library(colorspace)
library(shinyWidgets)
library(DT)
library(fixest)
library(modelsummary)
library(huxtable)

# Data #########################################################################
here()
Ukraine_GM <- read.csv(here("Data", "Ukraine_Gravity_Model.csv"))
Ukraine_GM_noNA <- Ukraine_GM %>%
  drop_na()

Ukraine_data_graph <- Ukraine_GM %>%
  filter(Country == "Ukraine") %>%
  select(Country, Country_Code, Year, GDP, Latitude, Longitude, PR, CL)

Graph_data <- bind_rows(Ukraine_GM_noNA, Ukraine_data_graph)

Other_countries <- Graph_data %>%
  filter(Country != "Ukraine") %>%
  pull(Country) %>%
  unique()

Russosphere <- c("Azerbaijan", "Armenia", "Belarus", "Kyrgyzstan", 
                 "Tajikistan", "Turkmenistan", "Uzbekistan", "Russian Federation")

EU <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic",
        "Denmark", "Estonia", "Finland", "France", "Germany", "Greece",
        "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
        "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia",
        "Slovenia", "Spain", "Sweden")

Gisco_countries <- gisco_get_countries(year = 2024)

# User interface ###############################################################
ui <- page_navbar(
  title = "Ukraine Trade Shift Project",
  
  ## PAGE 1
  nav_panel(
    title = "Introduction",
    h1("Ukraine Trade Shift Project"),
    
    h2("Overview"),
    p("This interactive dashboard explores how Ukraine's international trade has shifted over time, 
     particularly in response to major political events such as the 2014 Euromaidan protests. The analysis applies the Gravity Model of Trade to understand economic 
     realignment in relation to political and institutional factors."),
    
    h2("Key Questions Explored in this Dashboard"),
    tags$ul(
      tags$li("How has Ukraine's trade with Russia changed since 2014?"),
      tags$li("To what extent has trade shifted toward the European Union?"),
      tags$li("What role do political rights and civil liberties play in Ukraine’s trade patterns?"),
      tags$li("How has bilateral trade with specific countries evolved over time?"),
      tags$li("What do regression models tell us about the impact of institutional factors on trade?")
    ),
    
    h2("How to Use This Dashboard"),
    tags$ul(
      tags$li("Navigate between different panels using the tabs at the top."),
      tags$li("Use the sliders to adjust the year and explore changes over time."),
      tags$li("Select a country from the dropdown menu to analyze bilateral trade."),
      tags$li("Review regression results to understand deeper economic relationships.")
    ),
    
    h2("Dashboard Features"),
    tags$ul(
      tags$li(strong("Foreign Trade Volume:"), " View Ukraine's total trade volume with different countries."),
      tags$li(strong("Institutional Freedoms:"), " See how political rights and civil liberties change over time."),
      tags$li(strong("Bilateral Analysis:"), " Compare Ukraine’s trade relationships with specific countries."),
      tags$li(strong("Shift to the West:"), " Visualize Ukraine’s trade divergence from Russia and integration with the EU."),
      tags$li(strong("Regression Results:"), " Explore the econometric results from the Gravity Model of Trade."),
      tags$li(strong("Data Browser:"), " Explore the underlying dataset.")
    ),
    
    h2("Why This Matters"),
    p("Ukraine's trade patterns are deeply intertwined with its geopolitical shifts. This dashboard provides a 
     data-driven exploration of how economic relationships have evolved as Ukraine has aligned more closely 
     with Western economies. By leveraging data from the World Bank, Freedom House, and geospatial sources, 
     this analysis provides insights into the broader implications of trade realignment."),
    
    hr()
  ),
  
  ## Page 2
  nav_panel(
    title = "About the Project",
    h1("Trade and Realignment: Ukraine’s Shift in Trade and Politics"),
    
    h2("Research Overview"),
    p("This dashboard is based on the study 'Trade and Realignment'. 
     It examines how Ukraine’s international trade relationships have changed in response to political events. 
     The study focuses on Ukraine’s transition away from economic dependence on Russia and toward greater integration with the European Union.
     The analysis covers trade patterns from 1996 to 2022, with special attention to the impact of the 2014 Euromaidan protests and the subsequent geopolitical shifts."),
    
    h2("Political Context and Motivation"),
    p("Since gaining independence from the Soviet Union in 1991, Ukraine has undergone significant political and economic transformations. 
     Initially, its economy remained closely tied to Russia due to historical trade relationships and political influence. 
     However, tensions between pro-European and pro-Russian political factions led to moments of instability, culminating in the 2014 Euromaidan protests 
     and the annexation of Crimea by Russia. These events marked a turning point, triggering a major political and economic realignment."),
    
    p("Before 2014, Ukraine’s trade was heavily dependent on Russia and other post-Soviet states. 
     After 2014, this pattern changed significantly, with Ukraine expanding its trade relationships with the European Union and other democratic, market-oriented economies."),
    
    h2("Methodology: The Gravity Model of Trade"),
    p("To analyze Ukraine’s trade realignment, the study applies the Gravity Model of Trade, a widely used economic framework that explains trade flows between countries 
     based on their economic size (GDP) and the distance between them. The model incorporates additional variables, such as political rights, civil liberties, 
     and institutional freedom scores, to assess how non-economic factors shape trade patterns."),
    
    p("This analysis utilizes panel data from 185 countries between 1996 and 2022, drawing from multiple data sources:"),
    tags$ul(
      tags$li("Trade data: Collected from the World Integrated Trade Solution (WITS) database by the World Bank."),
      tags$li("GDP data: Sourced from the World Bank’s World Development Indicators (WDI)."),
      tags$li("Institutional and political freedom indicators: Derived from Freedom House Index, ranking countries on Political Rights and Civil Liberties."),
      tags$li("Geographical data: Capital-to-capital distances calculated using latitude and longitude coordinates verified from the CIA World Factbook.")
    ),
    
    h2("Key Findings"),
    tags$ul(
      tags$li(strong("Trade Dependence on Russia Declined:"), "Before 2014, Russia was Ukraine’s largest trading partner. After 2014, trade volumes dropped sharply."),
      tags$li(strong("Shift Toward the European Union:"), "Trade with EU countries increased significantly, particularly with Poland and Germany"),
      tags$li(strong("Institutional Freedom and Trade Relationships:"), "Ukraine’s trade increasingly favoured countries with stronger political rights and civil liberties."),
      tags$li(strong("Effects of War in 2022:"), "The Russian invasion of Ukraine in 2022 further disrupted trade, accelerating Ukraine’s economic separation from Russia.")
    ),
    hr()
  ),
  
  
  ## PAGE 3
  nav_panel(
    title = "Foreign Trade Volume",
    h1("Ukraine's Foreign Trade Volume"),
    plotOutput(outputId = "DB_MAP"),
    layout_column_wrap(
      width = 1/2,
    # Year Slider
    sliderInput(
        inputId = "year_select",
        label = "Year Selector",
        min = min(Ukraine_GM_noNA$Year), max = max(Ukraine_GM_noNA$Year),
        step = 1,
        value = 1996,
        sep = "",
        width = "100%",
        animate = animationOptions(interval = 750, loop = TRUE, playButton = NULL,
                                   pauseButton = NULL)
    ),
    # Type of Map input
    selectInput(
      "type_select",
      "Type of Map",
      choices = c("Trade Volume in $US", "Trade Volume in %" ),
      multiple = FALSE,
      selected = "Trade Volume in $US"
    ))
  ),
  
  ## PAGE 4
  nav_panel(
    title = "Institutional Freedoms",
    tabsetPanel(
      #Panel 1
      tabPanel(
        title = "Political Rights",
        plotOutput(outputId = "PR_MAP"),
        sliderInput(
          inputId = "year_select2",
          label = "Year Selector",
          min = min(Ukraine_GM_noNA$Year), max = max(Ukraine_GM_noNA$Year),
          step = 1,
          value = 1996,
          sep = "",
          width = "50%",
          animate = animationOptions(interval = 750, loop = TRUE, playButton = NULL,
                                     pauseButton = NULL)
        )
      ),
      #Panel 2
      tabPanel(
        title = "Civil Liberties",
        plotOutput(outputId = "CL_MAP"),
        sliderInput(
          inputId = "year_select3",
          label = "Year Selector",
          min = min(Ukraine_GM_noNA$Year), max = max(Ukraine_GM_noNA$Year),
          step = 1,
          value = 1996,
          sep = "",
          width = "50%",
          animate = animationOptions(interval = 750, loop = TRUE, playButton = NULL,
                                     pauseButton = NULL)
        )
      )
    )
  ),
  ## PAGE 5
  nav_panel(
    title = "Bilateral Analysis",
    h1("Bilateral Analysis"),
    layout_columns(
      col_widths = c(8, 4),
      plotOutput(outputId = "BILA_MAP"),
      plotOutput(outputId = "BILA_PR"),
      plotOutput(outputId = "BILA_TRADE"),
      plotOutput(outputId = "BILA_CL")
  )),
  
  ## PAGE 6
  nav_panel(
    title = "Shift to the West",
    h1("Shift to the West"),
    layout_columns(
      col_widths = c(8, 4),
      plotOutput(outputId = "SHIFT_MAP"),
      plotOutput(outputId = "SHIFT_PR"),
      plotOutput(outputId = "SHIFT_TRADE"),
      plotOutput(outputId = "SHIFT_CL")
    )),
  
  ## PAGE 7
  nav_panel(
    title = "Regression Results",
    h1("Regression Results"),
    uiOutput("regression_table")
  ),
  
  
  ## PAGE 8
  nav_panel(
    title = "Data Browser",
    h1("Data Browser"),
    dataTableOutput(outputId = "table_overview")
  ),

  # Space in header
  nav_spacer(),
  
  # Sidebar
  sidebar = sidebar(
    "Bilateral Trade Selector",
    selectInput(
      "country_select",
      "Country",
      choices = Other_countries,
      multiple = FALSE,
      selected = "United States"
    ),
    open = "open"
  ),
  
  # Options
  theme = bs_theme(
    bootswatch = "zephyr" 
  )
)


# Server #######################################################################
server <- function(input, output, session) {

  ## Reactive objects ##########################################################
  
  UGM_map_reactive <- reactive({
    UGM_map_temp <- left_join(Gisco_countries, 
                              Graph_data %>% 
                                filter(Year == input$year_select) %>%
                                mutate(binned = 
                                         if(input$type_select == "Trade Volume in $US"){
                                           cut(Total,
                                               breaks = c(0, 10000, 100000, 1000000, 5000000, 10000000, 100000000),
                                               dig.lab = 5)} 
                                       else {cut(Total/World_Trade_Volume*100,
                                                 breaks = c(0, 1, 5, 10, 15, 30, 100),
                                                 dig.lab = 5)}),
                              by = c("ISO3_CODE" = "Country_Code"))
  })
  
  labs_plot_reactive <- reactive({
    labs_plot_temp <- if(input$type_select == "Trade Volume in $US"){
                               c("(<10m)", "(10m-100m)", "(100m-1b)", "(1b-5b)", "(5b-10b)", "(>10b)")} 
    else {c("(<1%)", "(1%-5%)", "(5%-10%)", "(10%-15%)", "(15%-30%)", "(>30%)")}
  })
  
  legend_name_reactive <- reactive({
    labs_plot_temp <- if(input$type_select == "Trade Volume in $US"){
      "US$"} else {c("%")}
  })
  
  PR_map_reactive <- reactive({
    PR_map_temp <- left_join(Gisco_countries, 
                              Graph_data %>% 
                                filter(Year == input$year_select2),
                              by = c("ISO3_CODE" = "Country_Code"))
  })

  CL_map_reactive <- reactive({
    CL_map_temp <- left_join(Gisco_countries, 
                              Graph_data %>% 
                                filter(Year == input$year_select3),
                              by = c("ISO3_CODE" = "Country_Code"))
  })
  
  bilateral_onmap_reactive <- reactive({
    bilateral_onmap_temp <- gisco_get_countries(year = 2024, country = input$country_select)  
  })
  
  BILA_map_reactive <- reactive({
    BILA_map_temp <- left_join(Gisco_countries, 
                        Graph_data %>%
                          filter(Country == input$country_select), 
                        by = c("ISO3_CODE" = "Country_Code"))
  })
  
  BILA_reactive <- reactive({
    BILA_temp <- Graph_data %>%
      filter(Country == input$country_select)
  })
  
  ## Tables ####################################################################
  
  #Data Browser
  
    output$table_overview <- renderDT({
    Ukraine_GM_noNA %>%
      filter(Country != "Ukraine") %>%
      select(-UKR_GDP, -UKR_LAT, -UKR_LON, -UKR_PR, -UKR_CL) %>%
      rename(ISO_Code = Country_Code)
  })
  
  #Regression Table
  
  output$regression_table <- renderUI({
    
    # Ensure dataset is cleaned before regressions
    Ukraine_GM_clean <- Ukraine_GM_noNA %>%
      filter(!is.na(Total) & !is.na(GDP) & !is.na(UKR_GDP) & !is.na(Distance)) 
    
    # Base Gravity Model
    Base_Gravity_Model <- feols(log(Total) ~ log(GDP) + log(UKR_GDP) + log(Distance), 
                                data = Ukraine_GM_clean)
    
    # Institutional Freedoms Model
    Institutional_Freedoms_Model <- feols(
      log(Total) ~ log(GDP) + log(UKR_GDP) + log(Distance) + Post + CLPR + UKR_CLPR + I(CLPR * Post),
      data = Ukraine_GM_clean %>%
        mutate(Post = ifelse(Year >= 2014, 1, 0),
               CLPR = 0.5 * (CL + PR),
               UKR_CLPR = 0.5 * (UKR_CL + UKR_PR)) %>%
        filter(!is.na(CLPR) & !is.na(UKR_CLPR))
    )
    
    # Grouped Countries Model
    Grouped_Countries_Model <- feols(
      log(Total) ~ log(GDP) + log(UKR_GDP) + log(Distance) + Russosphere + EU + Post + I(EU * Post) + I(Russosphere * Post),
      data = Ukraine_GM_clean %>%
        mutate(Post = ifelse(Year >= 2014, 1, 0),
               Russosphere = ifelse(Country %in% Russosphere, 1, 0),
               EU = ifelse(Country %in% EU, 1, 0))
    )
    
    # Generate a clean regression table using huxtable (instead of kableExtra or gt)
    table_output <- modelsummary(
      list("Base Gravity Model" = Base_Gravity_Model,
           "Institutional Freedoms Model" = Institutional_Freedoms_Model,
           "Grouped Countries Model" = Grouped_Countries_Model),
      stars = TRUE,  # Adds significance stars
      output = "huxtable"
    )
    
    # Apply styling to remove unnecessary spaces
    table_output <- huxtable::set_width(table_output, 0.8) %>%  # Adjust width
      huxtable::set_bold(1, everywhere, TRUE) %>%  # Bold header row
      huxtable::set_font_size(10) %>%  # Set font size
      huxtable::set_caption("Regression Analysis of Ukraine’s Trade Shift")  # Title
    
    # Render the table as an HTML object
    HTML(huxtable::to_html(table_output))
  })

  ## Graphs ####################################################################
  
  #Trade Volume Map
  
  ukraine_onmap <- gisco_get_countries(year = 2024, country = "Ukraine")
  
  pal <- hcl.colors(6, "YlOrRd", rev = TRUE)
  
  output$DB_MAP <- renderPlot({
    UGM_map_reactive() %>%
      ggplot(aes(fill = binned)) +
      geom_sf(color = NA) +
      geom_sf(data = ukraine_onmap, fill = "deepskyblue", color = "gray10") +
      labs(fill = legend_name_reactive()) +
      scale_fill_manual(values = pal,
                        drop = FALSE,
                        na.value = "grey65",
                        label = labs_plot_reactive()) +
      theme_bw() +
      theme(legend.key.size = unit(1, "cm"),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 14),
            panel.background = element_rect(fill  = "azure"))
  }) %>% bindCache(input$year_select)
  

  #Institutional Freedoms Maps
  
  pal2 <- hcl.colors(7, "Red-Blue", rev = TRUE)
  
  output$PR_MAP <- renderPlot({
    PR_map_reactive() %>%
      ggplot(aes(fill = as.factor(PR))) +
      geom_sf(color = NA) +
      labs(fill = "PR index") +
      scale_fill_manual(values = pal2,
                        drop = FALSE,
                        na.value = "grey65") +
      theme_bw() +
      theme(legend.key.size = unit(1, "cm"),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 14),
            panel.background = element_rect(fill  = "azure"))
    }) %>% bindCache(input$year_select2)
  
  output$CL_MAP <- renderPlot({
    CL_map_reactive() %>%
      ggplot(aes(fill = as.factor(CL))) +
      geom_sf(color = NA) +
      labs(fill = "CL index") +
      scale_fill_manual(values = pal2,
                        drop = FALSE,
                        na.value = "grey65") +
      theme_bw() +
      theme(legend.key.size = unit(1, "cm"),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 14),
            panel.background = element_rect(fill  = "azure"))
  }) %>% bindCache(input$year_select3)
  
  # Bilateral Trade Map
  
  BILA_map <- left_join(gisco_get_countries(year = 2024), 
                        Graph_data, 
                        by = c("ISO3_CODE" = "Country_Code"))
  
  pos_or_neg <- function(input){
    ifelse(as.numeric(input) >= 0, 1, -1)
  }
  
  output$BILA_MAP <- renderPlot({
    BILA_map %>%
      ggplot() +
      geom_sf(color = NA) +
      geom_sf(data = ukraine_onmap, fill = "deepskyblue", color = "gray10") +
      geom_sf(data = bilateral_onmap_reactive(), fill = "red3", color = "gray10") +
      theme_bw() +
      theme(panel.background = element_rect(fill  = "azure")) +
      geom_curve(data = BILA_reactive(), 
                 aes(x = Longitude, y = Latitude, xend = UKR_LON, yend = UKR_LAT), 
                 color = "gray10", 
                 curvature = 0.3 * pos_or_neg(
                   unique(BILA_reactive()$Longitude - BILA_reactive()$UKR_LON)), 
                 size = 0.5)
  })
  
  # Institutional Variables Graph
  
  country_colors <- c("Ukraine" = "deepskyblue", setNames(rep("red3", length(Other_countries)), Other_countries))
  
  output$BILA_CL <- renderPlot({
    Graph_data %>%
      filter(Country == "Ukraine") %>%
      ggplot(aes(x = Year, color = Country)) +
      geom_line(data = BILA_reactive(), aes(y = CL), size = 1) +
      geom_line(aes(y = CL), size = 1) +
      geom_vline(xintercept = 2014, linetype = "dashed") +
      annotate("text", label = "Euromaidan", x = 2014.5, y = 4.5, angle = 90) +
      theme_bw() +
      labs(y = "Civil Liberties Index",
           title = "Civil Liberties Indices over Time") +
      scale_color_manual(
        values = country_colors) +
      scale_y_continuous(breaks = seq(1, 7, by = 1), limits = c(1, 7)) +
      theme(legend.position = "bottom")
  })
  
  output$BILA_PR <- renderPlot({
    Graph_data %>%
      filter(Country == "Ukraine") %>%
      ggplot(aes(x = Year, color = Country)) +
      geom_line(data = BILA_reactive(), aes(y = PR), size = 1) +
      geom_line(aes(y = PR), size = 1) +
      geom_vline(xintercept = 2014, linetype = "dashed") +
      annotate("text", label = "Euromaidan", x = 2014.5, y = 4.5, angle = 90) +
      theme_bw() +
      labs(y = "Political Rights Index",
           title = "Political Rights Indices over Time") +
      scale_color_manual(
        values = country_colors) +
      scale_y_continuous(breaks = seq(1, 7, by = 1), limits = c(1, 7)) +
      theme(legend.position = "bottom")
  })
  
  # Bilateral Trade Graph
  output$BILA_TRADE <- renderPlot({
    BILA_reactive() %>%
      ggplot(aes(x = Year, y = Total/1000000, color = Country)) +
      geom_line(size = 1) +
      geom_vline(xintercept = 2014, linetype = "dashed") +
      annotate("text", label = "Euromaidan", x = 2014.5, y = 0, hjust = 0, angle = 90) +
      theme_bw() +
      labs(y = "Bilateral Trade Volume in Millions",
           title = "Bilateral Trade Volume over Time") +
      scale_y_continuous() +
      scale_color_manual(values = "red3") +
    theme(legend.position = "")
  })
 
  # Shift to the West Map
  EU_onmap <- gisco_get_countries(year = 2024, country = EU)
  RUSSOSPHERE_onmap <- gisco_get_countries(year = 2024, country = Russosphere)
    
  SHIFT_map <- gisco_get_countries(year = 2024)
  
  output$SHIFT_MAP <- renderPlot({
    SHIFT_map %>%
      ggplot() +
      geom_sf(color = NA) +
      geom_sf(data = EU_onmap, fill = "deepskyblue", color = "gray10") +
      geom_sf(data = RUSSOSPHERE_onmap, fill = "red3", color = "gray10") +
      geom_sf(data = ukraine_onmap, fill = "yellow3", color = "gray10") +
      theme_bw() +
      theme(panel.background = element_rect(fill  = "azure")) +
      coord_sf(xlim = c(-15, 80), ylim = c(35, 70))
  })

  # Shift to the West Institutional Variables Graph

  output$SHIFT_CL <- renderPlot({
    Graph_data %>%
      filter(Country %in% EU | Country %in% Russosphere) %>%
      mutate(Region = ifelse(Country %in% EU, "EU", "Russosphere")) %>%
      group_by(Year, Region) %>%
      summarize(Avg_CL = mean(CL)) %>%
      ggplot(aes(x = Year, y = Avg_CL, color = Region)) +
      geom_line(size = 1) +
      geom_vline(xintercept = 2014, linetype = "dashed") +
      annotate("text", label = "Euromaidan", x = 2014.5, y = 4.5, angle = 90) +
      theme_bw() +
      labs(y = "Unweighted Average of Civil Liberties Indices",
           title = "Civil Liberties Indices over Time") +
      scale_y_continuous(breaks = seq(1, 7, by = 1), limits = c(1, 7)) + 
      scale_color_manual(values = c("EU" = "deepskyblue", "Russosphere" = "red3")) +
      theme(legend.position = "bottom") 
  })
  
  output$SHIFT_PR <- renderPlot({
    Graph_data %>%
      filter(Country %in% EU | Country %in% Russosphere) %>%
      mutate(Region = ifelse(Country %in% EU, "EU", "Russosphere")) %>%
      group_by(Year, Region) %>%
      summarize(Avg_PR = mean(PR)) %>%
      ggplot(aes(x = Year, y = Avg_PR, color = Region)) +
      geom_line(size = 1) +
      geom_vline(xintercept = 2014, linetype = "dashed") +
      annotate("text", label = "Euromaidan", x = 2014.5, y = 4.5, angle = 90) +
      theme_bw() +
      labs(y = "Unweighted Average of Political Rights Indices",
           title = "Political Rights Indices over Time") +
      scale_y_continuous(breaks = seq(1, 7, by = 1), limits = c(1, 7)) + 
      scale_color_manual(values = c("EU" = "deepskyblue", "Russosphere" = "red3")) +
      theme(legend.position = "bottom") 
  })
  
  # Shift to the West Trade Graph
  
  Russosphere_Total <- Graph_data %>%
    filter(Country %in% Russosphere) %>%
    select(Year, Total, Country, World_Trade_Volume) %>%
    pivot_wider(names_from = Country,
                values_from = Total) %>%
    group_by(Year) %>%
    mutate(Russosphere = rowSums(across(Armenia:Uzbekistan))) %>%
    select(Year, World_Trade_Volume, Russosphere)
  
  EU_Total <- Graph_data %>%
    filter(Country %in% EU) %>%
    select(Year, Total, Country) %>%
    pivot_wider(names_from = Country,
                values_from = Total) %>%
    group_by(Year) %>%
    replace(is.na(.), 0) %>%
    mutate(EU = rowSums(across(Austria:Sweden))) %>%
    select(Year, EU)
  
  output$SHIFT_TRADE <- renderPlot({
    Russosphere_Total %>%
    left_join(EU_Total) %>%
      pivot_longer(Russosphere:EU, 
                   names_to = "Region", 
                   values_to = "Total") %>%
      group_by(Year) %>%
      mutate(Percentage = Total/World_Trade_Volume) %>%
      ggplot(aes(x = Year, y = Percentage, color = Region)) +
      geom_line(size = 1) +
      geom_vline(xintercept = 2014, linetype = "dashed") +
      annotate("text", label = "Euromaidan", x = 2014.5, y = 0.5, angle = 90) +
      theme_bw() +
      labs(y = "Share of Ukraine's foreign trade volume",
           title = "Ukraine Foreign trade for EU and Russosphere") +
      scale_y_continuous(labels = label_percent()) +
      scale_color_manual(
        values = c("Russosphere" = "red3",
                   "EU" = "deepskyblue"))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
