library(shiny)
library(shinydashboard)
library(WDI)
library(ggplot2)
library(plotly)
library(dplyr)
library(extrafont)

 
###################################################################### UI #####################################################################

# Define UI for application that draws a histogram
ui <- dashboardPage(
    
    dashboardHeader(
        title = "Impact of COVID and War on Armenia",
        titleWidth = 450
    ),
    
    dashboardSidebar(width = 200, collapsed = TRUE,
                     sidebarMenu(
                       menuItem(tabName = "general", text = "General", icon = icon("house-user")),
                       menuItem(tabName = "gdp", text = "GDP", icon = icon("chart-line")),
                       menuItem(tabName = "debt", text = "Debt", icon = icon("dollar-sign"), badgeLabel = "map"),
                       menuItem(tabName = "military", text = "Military Expenditure", icon = icon("shield-alt")),
                       menuItem(tabName = "tour", text = "Tourism", icon = icon("umbrella-beach")),
                       menuItem(tabName = "covid", text = "Covid Cases", icon = icon("viruses")),
                       menuItem(tabName = "about", text = "About", icon = icon("adn")),
                       menuItem(tabName = "references", text = "References", icon = icon("bookmark"))
                     )),
    #skin = "red",
    
    dashboardBody(
        tabItems(
            
            ######## The UI of general ##########
            tabItem(tabName = "general",
                    fluidRow(valueBox("2,971,050", "Population", icon = icon("users"), color = "yellow"),
                             valueBox("14044.71 Million USD", "External Debt", icon = icon("dollar-sign"), color = "red"),
                             valueBox("4,267,45 USD ", "GDP per capita", icon = icon("chart-line"), color = "green"),
                             valueBox("338,000", "Confirmed COVID cases", icon = icon("viruses")),
                             valueBox("7,485", "COVID death cases", icon = icon("user"), color = "orange"),
                             valueBox("5000", "War Confirmed Deaths", icon = icon("shield-alt"), color = "purple"),
                             valueBox("12,645,459.21", "Current GDP", icon = icon("dollar-sign"), color = "yellow"),
                             valueBox("15%", "Unemployment Rate", icon = icon("address-card"), color = "green"),
                             valueBox("-7.6%", "GDP growth", icon = icon("dollar-sign"), color = "red"),
                             
                    ),  
                    
                    fluidRow( box(width = 12,plotlyOutput("plot1", height = 650), status = "primary")
                              
                              
                    )     
                    
                    
            ),
            
            ######## The UI of GDP ##########
            tabItem(tabName = "gdp",
                    fluidRow(tabItem(tabName = "GDP",
                                     fluidRow(box(width = 6, plotlyOutput("plot6", height = 600), status = "primary"),
                                              box(width = 6, plotlyOutput("plot7", height = 600), status = "primary"))))), 
            ######## The UI of debt ##########
            tabItem(tabName = "debt",
                    fluidRow(box(width = 12, plotlyOutput("map", height = 700), 
                                 title = "Map on External Debt Stocks in Asia (%)", 
                                 status = "primary", solidHeader = TRUE))),
            
            ######## The UI of military ##########
            tabItem(tabName = "military",
                    fluidRow(box(width = 6, plotlyOutput("fig12", height = 700),
                                 status = "primary"),
                             box(width = 6, plotlyOutput("fig22", height = 700),
                                 status = "primary"))
            ),
            
            ######## The UI of tour ##########
            tabItem(tabName = "tour",
                    fluidRow(column(width = 12, height = 300, 
                                    box(width = 12, height = 400, 
                                        plotlyOutput("tourism", height = 400), status = "primary")),
                             dataTableOutput("dat1")
                             
                    )
            ),
            tabItem(tabName = "covid",
                    fluidRow(column(width = 12, height = 400, 
                                    box(width = 12, height = 500, 
                                        plotlyOutput("covidplot", height = 400),
                                        status = "primary", 
                                        title = "Daily Covid Cases", solidHeader = TRUE)))),
            tabItem(tabName = "references", 
                    fluidRow(box(
                      title = "Data", width = 12, 
                      "The input data for this dashboard is the dataset available from the World Bank Data and https://github.com/RamiKrispin/coronavirus"
                    ),
                    box(title = "Code", width = 12, 
                        "The code behind this dashboard is available on GitHub: https://github.com/HripyVoskanyan/Econ_Dashboard_Group_1"
                        
                    ),
                    box(title = "Learning more about Plotly and Shiny", width = 12, 
                        "To learn more about Plotly and Shiny packages which were used to build this dashboard you can see:\n 
                        https://plotly.com/r/ and https://rstudio.github.io/shinydashboard/index.html"
                        
                    ),
                    box(title = "Sources", width = 12, 
                        HTML("<ul><li>https://rpubs.com/DataPointArmenia/Covid19ArmeniaDashboard</li><li>https://rstudio.github.io/shinydashboard/examples.html</li><li>https://shiny.rstudio.com/gallery/</li><li>https://iopscience.iop.org/article/10.1088/1755-1315/546/3/032043/pdf</li><li>...https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7505605/...</li></ul>")
                        
                    ),
                    box(title = "Further Improvements", width = 12, 
                        HTML("<ul><li>Code Cleaning</li><li>Use more indicators</li></ul>")
                        
                    ),
                    box(title = "Contacts", width = 12,
                        HTML("<ul><li>hripsime_voskanyan@edu.aua.am</li><li>gogo_hovhannisyan@edu.aua.am</li><li>sona_stepanyan@edu.aua.am</li><li>lusine_babayan@edu.aua.am</li></ul>")))),

            tabItem(tabName = "about",
                    
                    # fluidRow()
                    fluidRow(box(width = 4,height = 900,img(h2("About Us"), h5("We are juniors majoring in data science (DS) at the American University of Armenia. 
                    As Armenia has experienced recent economic shocks lately, including Covid and the 2020 Armenia-Azerbaijan war, we cooperated with 
                    DataPoint Armenia to do a project to understand the economic health of Armenia.Our goal was to create an easy-to-use interactive economic 
                    dashboard to make Armenia's financial data more readily available."), 
                    h5 ("We wanted to apply the knowledge we gained during our data visualization course to reach our goal.
                    Although we were familiar with the R language and knew how to create telling graphs, 
                        we were new to build dashboard and interactive plots."),  h2("DataPoint Armenia"),
                    h5("DataPoint Armenia is a group of students and professionals passionate about data science (DS). 
                       Their aim is to expand awareness & collaboration in DS to support a culture of evidence-based decision-making in Armenia."), 
                    h2("The Dashboard"), h5("This dashboard tells us about the impact of Covid-19 and the War on Armenia, mainly from an economic perspective. 
                                            We used data and advanced statistical models to create this dashboard to measure the economic impact. Here, 
                                            you can also find the data about neighboring countries of Armenia and compare them with each other. "),
                    h5("By analyzing the data that the dashboard includes, we can have some insights into how Armenia would be today without the war 
                       and how the world would be today without the Covid-19. "), h2("Linkedin Links")), 
                    tags$li(class = "dropdown", tags$a(href = "http://www.linkedin.com/in/lusine-babayan-a58938224", icon("linkedin"), "Linkedin Page (Lusine)")),
                    tags$li(class = "dropdown", tags$a(href = "http://www.linkedin.com/in/hripsime-voskanyan", icon("linkedin"), "Linkedin Page (Hripsime)")),
                    tags$li(class = "dropdown", tags$a(href = "http://www.linkedin.com/in/sona-stepanyan-ba28981ba", icon("linkedin"), "Linkedin Page (Sona)")),
                    tags$li(class = "dropdown", tags$a(href = "http://www.linkedin.com/in/gogo-hovhannisyan-00a814134", icon("linkedin"), "Linkedin Page (Gogo)"))),
                   
                            box(width = 8, height = 400, img(width = 6, height = 6, src = "AUA.png", imageOutput('aua')),
                                                         ),
                            box(width = 4, height = 400, img(width = 6, height = 6, src = "data.png", imageOutput('image'))
                                                         ),
                            box(width = 4, height =250, img(width = 6, height = 6, src = "enlight.png", imageOutput('dat')))
      

                    )
                    
            )
        )
    )
)
        






########################################################### Server ############################################################################

# Define server logic required to draw a histogram
server <- function(input, output)  {
   

  
  ######## The server of general ##########
  output$plot1 <- renderPlotly({
    data1 <- WDI(indicator='NY.GDP.PCAP.CD', country=c('ARM','AZE','TUR', 'IRN', 'TUR', 'JOR'), start=1990, end=2020)
    colnames(data1) <- c('Country_name', 'Country', 'Receipts', 'Year')

    data1$Country <- as.factor(data1$Country)
    levels(data1$Country)
    

    
    gdp <- ggplotly(ggplot(data1, aes(Year, Receipts, color=Country)) + geom_line() + geom_point() +
                      xlab('Year') + ylab('GDP per capita ($)') + theme_bw() +
                      ggtitle('GDP per capita (current US$)') +  expand_limits(y =c(0, 15000)) +
                      scale_x_continuous(breaks = round(seq(min(data1$Year), max(data1$Year), by = 1),1)) +
                      scale_y_continuous(labels = scales::comma) +
                      scale_color_manual(values = c("Armenia" = "#E51616", 
                                                    "Azerbaijan" = "#1128BF", 
                                                    "Iran, Islamic Rep." = "#19C40A",
                                                    "Jordan" = "#EE9E1C", 
                                                    "Turkey" = "#FF00B9")) +
                      theme(
                        plot.title = element_text(hjust = 0.5),
                        axis.title.x = element_text(face="bold"),
                        axis.title.y = element_text(face="bold", hjust = 3))
                    
    )
    
    gdp
  })
  

  output$tourism <- renderPlotly({
    data1 <- WDI(indicator='ST.INT.ARVL', country=c('ARM', 'AZE', 'IRN', 'JOR', 'CYP'), start=1990, end=2020)
    colnames(data1) <- c('Country_id', 'Country', 'arrivals', 'Year')
    
    
    
    #Adding missing data of 2020
    
    #Armenia
    data1["1", "arrivals"] <- 375216
    
    #Iran
    data1["94", "arrivals"] <- 512000
    
    #Jordan
    data1["125", "arrivals"] <- 741603
    
    #Cyprus
    data1["63", "arrivals"] <- 512184
    
    #Azerbaijan
    data1["63", "arrivals"] <- 542000
    
    
    
    data1$Country <- as.factor(data1$Country)
    levels(data1$Country)
    
    arrivals <- ggplotly(ggplot(data1, aes(Year, arrivals, color = Country)) +
                           xlab('Year') + ylab("The number of arrivals") + theme_bw() + ggtitle("The relationship between year and the number of arrivals") +
                           geom_line() + geom_point() + expand_limits(y =c(0, 10000000)) +
                           scale_x_continuous(breaks = round(seq(min(data1$Year), max(data1$Year), by = 1),1)) +
                           scale_y_continuous(labels = scales::comma) +
                           scale_color_manual(values = c("Armenia" = "#E51616", 
                                                         "Azerbaijan" = "#EE9E1C", 
                                                         "Iran, Islamic Rep." = "#19C40A",
                                                         "Jordan" = "#FF00B9", 
                                                         "Cyprus" = "#1128BF")) +
                           theme(
                             plot.title = element_text(hjust = 0.5),
                             axis.title.x = element_text(face="bold"),
                             axis.title.y = element_text(face="bold", hjust = 3))
                         
    )
    
    arrivals
    
  })
  ######## The server of GDP1 ##########
  output$plot6 <- renderPlotly({
    df6 <- data.frame(WDI(indicator='NY.GDP.MKTP.CD', country=c('ARM','AZE', 'GEO','TJ', 'AF', 'LBN'), start=1990, end=2020))
    
    fig6 <- plot_ly(df6, x = ~year, y = ~NY.GDP.MKTP.CD, color = ~country, type = "scatter", mode = "lines+markers")
    fig6 <- fig6 %>% layout(title = "GDP of Armenia and Neighboring Countries",
                            yaxis = list(title = 'GDP (current US$)', showgrid=FALSE),
                            xaxis= list(title=FALSE, showgrid=FALSE)
    )
    
    fig6
    
  })
  
  ######## The server of GDP2 ##########
  output$plot7 <- renderPlotly({
    test <- WDI(indicator='NY.GDP.MKTP.KD.ZG', country=c('ARM','AZE', 'GEO','TJ', 'AF', 'LBN','TUR'), start=1990, end=2020)
    abc <- plot_ly(z = ~test$NY.GDP.MKTP.KD.ZG, 
                   x = ~test$year, 
                   y = ~test$country,
                   type = "heatmap",
                   colors='RdYlGn',
                   colorbar = list(title = "Percentage change"))
    abc <- abc %>% layout(title = "GDP Growth of Armenia and Neighboring Countries",
                          yaxis = list(title = 'GDP growth (annual %)', showgrid=FALSE),
                          xaxis= list(title=FALSE, showgrid=FALSE))
    
    
    abc
    
  })
  
  
  ######## The server of debt ##########
  output$map <- renderPlotly({
    df <- WDI(indicator = "DT.DOD.DECT.GN.ZS", start = 2010, end = 2020)
    df <- df %>% mutate(hover = paste0(country, "\n ", round(DT.DOD.DECT.GN.ZS, digits = 2), "%"))
    
    fontStyle <- list(
      family = "DM Sans",
      size = 15,
      color = "black"
    )
    
    label <- list(
      bgcolor = "#EEEEEE",
      bordercolor = "transparent",
      font = fontStyle
    )
    
    p <- plot_geo(df, locationmode = 'country names', frame = ~year) %>%
      add_trace(locations = ~country, 
                z = ~ DT.DOD.DECT.GN.ZS,
                zmin = 10,
                zmax = 60,
                
                color = ~DT.DOD.DECT.GN.ZS,
                colorbar = list(title = "External Debt Stocks (%)"),
                text = ~hover,
                hoverinfo = "text") %>%
      layout(geo = list(scope = 'asia'), 
             font = list(family = "DM Sans"),
             legend = list(title = list(text = "<b> Debt in % </b>"))) %>%
      style(hoverlabel = label) %>%
      config(displayModeBar = FALSE) %>%
      colorbar(tickprefix = "%")  
    
  })
  
  ######## The server of military_1 ##########
  output$fig12 <- renderPlotly({
    dat = WDI(indicator='MS.MIL.XPND.CD', country=c('ARM','AZE', 'GEO', 'TUR', 'SYR', 'LBN', 'ISR', 'IRQ', 'LBY', 'AFG'), start=2015, end=2020)
    
    fig1 <- plot_ly(dat, x = ~year, y = ~MS.MIL.XPND.CD, color = ~country)
    fig1 <- fig1 %>% add_bars()
    fig1 <- fig1 %>% layout(title = 'Military Expenditure (current USD)', 
                            xaxis = list(title = "", showgrid = FALSE),
                            yaxis = list(title = "", showgrid = FALSE), 
                            barmode = 'stack')
    fig1
  })
  
  ######## The server of military_2 ##########
  output$fig22 <- renderPlotly({
    dat = WDI(indicator='MS.MIL.XPND.GD.ZS', country=c('ARM','AZE', 'GEO', 'TUR', 'SYR', 'LBN', 'ISR', 'IRQ', 'LBY', 'AFG'), start=2015, end=2020)
    
    fig2 <- plot_ly(dat, x = ~year, y = ~MS.MIL.XPND.GD.ZS, color = ~country)
    fig2 <- fig2 %>% add_bars()
    fig2 <- fig2 %>% layout(title = 'Military Expenditure (% of GDP)', 
                            xaxis = list(title = "", showgrid = FALSE),
                            yaxis = list(title = "", showgrid = FALSE), 
                            barmode = 'stack')
    
    fig2
  })
  
  data1 <- WDI(indicator='ST.INT.ARVL', country=c('ARM', 'AZE', 'IRN', 'JOR', 'CYP'), start=1995, end=2020)
  colnames(data1) <- c('Country_id', 'Country', 'arrivals', 'Year')
  
  #Adding missing data of 2020
  
  #Armenia
  data1["1", "arrivals"] <- 375216
  
  #Iran
  data1["94", "arrivals"] <- 512000
  
  #Jordan
  data1["125", "arrivals"] <- 741603
  
  #Cyprus
  data1["63", "arrivals"] <- 512184
  
  #Azerbaijan
  data1["63", "arrivals"] <- 542000
  
  
  
  data1$Country <- as.factor(data1$Country)
  levels(data1$Country)
  ######## The server of tourism ##########
  output$dat1 <- renderDataTable(data.frame(data1),
                                 options = list(
                                   pageLength = 5
                                   
                                 )
  )
  
  output$covidplot <- renderPlotly({
    coronavirus <- utils::read.csv("https://raw.githubusercontent.com/RamiKrispin/coronavirus/master/csv/coronavirus.csv", #header = TRUE,
                                   stringsAsFactors = FALSE)
    coronavirus$date <- base::as.Date(coronavirus$date)
    
    daily_confirmed <- coronavirus %>%
      dplyr::filter(type == "confirmed") %>%
      dplyr::filter(date >= "2020-02-29") %>%
      dplyr::mutate(country = country) %>%
      dplyr::group_by(date, country) %>%
      dplyr::summarise(total = sum(cases)) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(names_from = country, values_from = total)
    
    daily_confirmed %>%
      plotly::plot_ly() %>%
      plotly::add_trace(
        x = ~date,
        y = ~Armenia,
        type = "scatter",
        mode = "lines+markers",
        name = "Armenia"
      ) %>%
      plotly::add_trace(
        x = ~date,
        y = ~Azerbaijan,
        type = "scatter",
        mode = "lines+markers",
        name = "Azerbaijan"
      ) %>%
      plotly::add_trace(
        x = ~date,
        y = ~Syria,
        type = "scatter",
        mode = "lines+markers",
        name = "Syria"
      ) %>%
      plotly::add_trace(
        x = ~date,
        y = ~Georgia,
        type = "scatter",
        mode = "lines+markers",
        name = "Georgia"
      ) %>%
      plotly::add_trace(
        x = ~date,
        y = ~Iran,
        type = "scatter",
        mode = "lines+markers",
        name = "Iran"
      ) %>%
      plotly::add_trace(
        x = ~date,
        y = ~Lebanon,
        type = "scatter",
        mode = "lines+markers",
        name = "Lebanon"
      ) %>%
      plotly::add_trace(
        x = ~date,
        y = ~Cyprus,
        type = "scatter",
        mode = "lines+markers",
        name = "Cyprus"
      ) %>%
      plotly::layout(
        title = "",
        legend = list(x = 0.1, y = 0.9),
        yaxis = list(title = "New confirmed cases"),
        xaxis = list(title = "Date"),
        hovermode = "compare",
        margin = list(
          b = 10,
          t = 10,
          pad = 2
        )
      )
    
    
  })
    
    ####### The server of About ########
    output$image <- renderImage({
      list(src = "data.png",
           alt = "This is alternate text"
      )
    })

   output$aua <- renderImage({
     list(src = "AUA.png",
          alt = "This is alternate text"
      )
   })
   
   output$dat <- renderImage({
     list(src = "enlight.png",
          alt = "This is alternate text"
      )
   })
    
}    

# Run the application 
shinyApp(ui = ui, server = server)
