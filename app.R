#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(googleVis)

dispo_alim <- read.csv("C:/Users/muriel/Documents/IASchool/Prog_R/Projet/FAO_Project/CSV/Table/dispo_alim.csv")
under_nour <- read.csv("C:/Users/muriel/Documents/IASchool/Prog_R/Projet/FAO_Project/CSV/Table/undernourished.csv")
pop <- read.csv("C:/Users/muriel/Documents/IASchool/Prog_R/Projet/FAO_Project/CSV/Table/Population.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
    tabsetPanel(
        #--------------------------- Data Panel ----------------------------#
        tabPanel("Data",
                 titlePanel("DataSet"),
                 fluidRow(
                     #---------- Select list choice dataset -------------#
                     column(4, selectInput("dataset", 
                                           "Choisir le dataframe :",
                                           c("Dispo_alim" = "dispo",
                                             "Population" = "popu",
                                             "Sous-nutrition" = "under"), selected = "dispo"
                     )),
                     #---------- Select list choice year -------------#
                     column(4,
                            selectInput("y",
                                        "Year:",
                                        c("All",
                                          unique(as.character(dispo_alim$year))))
                     ),
                     #---------- Select list choice country -------------#
                     column(4,
                            selectInput("count",
                                        "Country:",
                                        c("All",
                                          unique(as.character(dispo_alim$country))))
                     ),
                     #---------- Select list choice origin -------------#
                     column(4,
                            selectInput("orig",
                                        "Origin:",
                                        c("All",
                                          unique(as.character(dispo_alim$origin))))
                     )
                 ),
                 DT::dataTableOutput("table"),
                 
        ), 
        #--------------------------- Graph Panel ----------------------------#
        tabPanel("Graph",
                 titlePanel("Représentation graphique"),
                 fluidRow(
                     #---------- Radio button choice dataset -------------#
                     column(4, 
                            radioButtons("dataset_c", 
                                         "Choisir le dataframe :",
                                         c("Dispo_alim" = "dispo_graph",
                                           "Population" = "popu_graph",
                                           "Sous-nutrition" = "under_graph"), selected = "dispo_graph"
                            )),
                     #---------- Radio button choice year -------------#
                     column(4,
                            radioButtons("y_graph",
                                         "Year:",
                                         c(unique(as.character(dispo_alim$year))), selected = "2014")
                     ),
                     #---------- Select list choice country -------------#
                     column(4,
                            selectInput("count_graph",
                                        "Country:",
                                        c("All",
                                          unique(as.character(dispo_alim$country))))
                     ),
                     #---------- Slider choice number of selection -------------#
                     column(4,sliderInput("slider1", label = h3("Nombre de selection"), min = 0, max = 100, value = 6))
                     
                     
                     
                     
                 ),
                 
                 
                 mainPanel(
                     plotOutput("plot")
                 )),
        #------------------------------------------------------ Carto Panel -----------------------------------------#
        tabPanel("Carto",
                 titlePanel("Cartographie"),
                 fluidRow(
                     #---------- Select list choice dataset -------------#
                     column(4, selectInput("dataset_carto", 
                                           "Choisir le dataframe :",
                                           c("Dispo_alim" = "dispo_carto",
                                             "Population" = "popu_carto",
                                             "Sous-nutrition" = "under_carto")
                     )),
                 ), 
                 mainPanel(
                     (htmlOutput("geoPlot"))
                 )
                 
        ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Filter data based on selections
    data <- reactive({
        if(input$dataset == "dispo"){
            data <- dispo_alim
            if (input$y != "All") {
                data <- data[data$year == input$y,]
            }
            if (input$count != "All") {
                data <- data[data$country == input$count,]
            }
            if (input$orig != "All") {
                data <- data[data$orig == input$orig,]
            }
        }
        else if (input$dataset == "popu"){
            data <- pop
            if (input$y != "All") {
                data <- data[data$Year == input$y,]
            }
            if (input$count != "All") {
                data <- data[data$Area == input$count,]
            }
        }
        else if (input$dataset == "under"){
            data <- under_nour        
            if (input$y != "All") {
                data <- data[data$Year == input$y,]
            }
            if (input$count != "All") {
                data <- data[data$Area == input$count,]
            }
        }
        return(data)
    })
    output$table <- DT::renderDataTable({data()})
    #created graph based on selections
    graph <- reactive({
        #------------------------------ dispo_alim traitement -----------------------#
        if (input$dataset_c == "dispo_graph"){
            if (input$count_graph == "All"){
                graph <- dispo_alim %>% group_by(country) %>% filter(year == input$y_graph) %>%
                    summarise(Disponibilité_alimentaire = sum(dispo_alim_tonnes)) %>% arrange(desc(Disponibilité_alimentaire)) %>% 
                    head(input$slider1) %>% ggplot(aes(x = country, y = Disponibilité_alimentaire)) +
                    geom_bar(stat = 'identity', fill="steelblue") +
                    geom_text(aes(label=Disponibilité_alimentaire), vjust=-0.3, size=3.5) +
                    theme_minimal()
            }
            else {
                graph <- dispo_alim %>% group_by(country) %>% filter(year == input$y_graph, country == input$count_graph) %>%
                    summarise(Disponibilité_alimentaire = sum(dispo_alim_tonnes)) %>% arrange(desc(Disponibilité_alimentaire)) %>% 
                    head(input$slider1) %>% ggplot(aes(x = country, y = Disponibilité_alimentaire)) +
                    geom_bar(stat = 'identity', fill="steelblue") +
                    geom_text(aes(label=Disponibilité_alimentaire), vjust=-0.3, size=3.5) +
                    theme_minimal()
                
            }
            
        }
        #------------------------------ Population traitement -----------------------#
        if (input$dataset_c == "popu_graph"){
                if (input$count_graph == "All"){
                    graph <- pop %>% group_by(country) %>% filter(year == input$y_graph) %>%
                        summarise(Population = sum(Value)) %>% arrange(desc(Population)) %>% head(input$slider1) %>% 
                        ggplot(aes(x = country, y = Population)) + geom_bar(stat = 'identity', fill="steelblue")+
                        geom_text(aes(label=Population), vjust=-0.3, size=3.5)+
                        theme_minimal()
                }
                else {
                    graph <- pop %>% group_by(country) %>% filter(year == input$y_graph, country == input$count_graph)%>%
                        summarise(Population = sum(Value)) %>% arrange(desc(Population)) %>% head(input$slider1) %>% 
                        ggplot(aes(x = country, y = Population)) + geom_bar(stat = 'identity', fill="steelblue")+
                        geom_text(aes(label=Population), vjust=-0.3, size=3.5)+
                        theme_minimal()
                    
                }
            
        }
        #------------------------------ Undernourished traitement -----------------------#
        if (input$dataset_c == "under_graph"){
            if (input$y_graph == "2014"){            
                if (input$count_graph == "All"){
                    graph <- under_nour %>% group_by(country) %>% filter(year == "2012-2014") %>%
                        summarise(Undenourished = sum(Value)) %>% arrange(desc(Undenourished)) %>% head(input$slider1) %>% 
                        ggplot(aes(x = country, y = Undenourished)) + geom_bar(stat = 'identity', fill="steelblue")+
                        geom_text(aes(label=Undenourished), vjust=-0.3, size=3.5)+
                        theme_minimal()
                }
                else {
                    graph <- under_nour %>% group_by(country) %>% filter(year == "2012-2014",country == input$count_graph) %>%
                        summarise(Undenourished = sum(Value)) %>% arrange(desc(Undenourished)) %>% head(input$slider1) %>% 
                        ggplot(aes(x = country, y = Undenourished)) + geom_bar(stat = 'identity', fill="steelblue")+
                        geom_text(aes(label=Undenourished), vjust=-0.3, size=3.5)+
                        theme_minimal()
                }
            }
            if (input$y_graph == "2015"){            
                if (input$count_graph == "All"){
                    graph <- under_nour %>% group_by(country) %>% filter(year == "2013-2015") %>%
                        summarise(Undenourished = sum(Value)) %>% arrange(desc(Undenourished)) %>% head(input$slider1) %>% 
                        ggplot(aes(x = country, y = Undenourished)) + geom_bar(stat = 'identity', fill="steelblue")+
                        geom_text(aes(label=Undenourished), vjust=-0.3, size=3.5)+
                        theme_minimal()
                }
                else {
                    graph <- under_nour %>% group_by(country) %>% filter(year == "2013-2015",country == input$count_graph) %>%
                        summarise(Undenourished = sum(Value)) %>% arrange(desc(Undenourished)) %>% head(input$slider1) %>% 
                        ggplot(aes(x = country, y = Undenourished)) + geom_bar(stat = 'identity', fill="steelblue")+
                        geom_text(aes(label=Undenourished), vjust=-0.3, size=3.5)+
                        theme_minimal()
                }
            }
            if (input$y_graph == "2016"){            
                if (input$count_graph == "All"){
                    graph <- under_nour %>% group_by(country) %>% filter(year == "2014-2016") %>%
                        summarise(Undenourished = sum(Value)) %>% arrange(desc(Undenourished)) %>% head(input$slider1) %>% 
                        ggplot(aes(x = country, y = Undenourished)) + geom_bar(stat = 'identity', fill="steelblue")+
                        geom_text(aes(label=Undenourished), vjust=-0.3, size=3.5)+
                        theme_minimal()
                }
                else {
                    graph <- under_nour %>% group_by(country) %>% filter(year == "2014-2016",country == input$count_graph) %>%
                        summarise(Undenourished = sum(Value)) %>% arrange(desc(Undenourished)) %>% head(input$slider1) %>% 
                        ggplot(aes(x = country, y = Undenourished)) + geom_bar(stat = 'identity', fill="steelblue")+
                        geom_text(aes(label=Undenourished), vjust=-0.3, size=3.5)+
                        theme_minimal()
                }
            }
            if (input$y_graph == "2017"){            
                if (input$count_graph == "All"){
                    graph <- under_nour %>% group_by(country) %>% filter(year == "2015-2017") %>%
                        summarise(Undenourished = sum(Value)) %>% arrange(desc(Undenourished)) %>% head(input$slider1) %>% 
                        ggplot(aes(x = country, y = Undenourished)) + geom_bar(stat = 'identity', fill="steelblue")+
                        geom_text(aes(label=Undenourished), vjust=-0.3, size=3.5)+
                        theme_minimal()
                }
                else {
                    graph <- under_nour %>% group_by(country) %>% filter(year == "2015-2017",country == input$count_graph) %>%
                        summarise(Undenourished = sum(Value)) %>% arrange(desc(Undenourished)) %>% head(input$slider1) %>% 
                        ggplot(aes(x = country, y = Undenourished)) + geom_bar(stat = 'identity', fill="steelblue")+
                        geom_text(aes(label=Undenourished), vjust=-0.3, size=3.5)+
                        theme_minimal()
                }
            }           
            if (input$y_graph == "2018"){            
                if (input$count_graph == "All"){
                    graph <- under_nour %>% group_by(country) %>% filter(year == "2016-2018") %>%
                        summarise(Undenourished = sum(Value)) %>% arrange(desc(Undenourished)) %>% head(input$slider1) %>% 
                        ggplot(aes(x = country, y = Undenourished)) + geom_bar(stat = 'identity', fill="steelblue")+
                        geom_text(aes(label=Undenourished), vjust=-0.3, size=3.5)+
                        theme_minimal()
                }
                else {
                    graph <- under_nour %>% group_by(country) %>% filter(year == "2016-2018",country == input$count_graph) %>%
                        summarise(Undenourished = sum(Value)) %>% arrange(desc(Undenourished)) %>% head(input$slider1) %>% 
                        ggplot(aes(x = country, y = Undenourished)) + geom_bar(stat = 'identity', fill="steelblue")+
                        geom_text(aes(label=Undenourished), vjust=-0.3, size=3.5)+
                        theme_minimal()
                }
            }
        }
        return(graph)
    })
    output$plot <- renderPlot({graph()})
    datasetInput <- reactive({
        switch (input$dataset_carto,
                "popu_carto" = pop,
                "dispo_carto" = dispo_alim,
                "under_carto" = under_nour
        )
    })
    output$geoPlot <- renderGvis({gvisGeoChart(datasetInput(), locationvar = "country")})
    
}

# Run the application 
shinyApp(ui = ui, server = server)
