#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(ggplot2)
library(shiny)
library(DT)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(shinythemes)
library(SwimmeR)

#Import data
dispo_alim <- read.csv("C:/Users/muriel/Documents/IASchool/Prog_R/Projet/FAO_Project/CSV/Table/dispo_alim.csv")
population <- read.csv("C:/Users/muriel/Documents/IASchool/Prog_R/Projet/FAO_Project/CSV/Table/Population.csv")
under <- read.csv("C:/Users/muriel/Documents/IASchool/Prog_R/Projet/FAO_Project/CSV/Table/undernourished.csv")
# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("FAO Data Analyse"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("choice", "Selectionner une vue :",
                        c("Population dans le monde (2014)" = "pop_2014",
                          "Population dans le monde (2015)" = "pop_2015",
                          "Population dans le monde (2016)" = "pop_2016",
                          "Population dans le monde (2017)" = "pop_2017",
                          "Population dans le monde (2018)" = "pop_2018",
                          "Population en sous-nutrition de 2015 à 2017" = "under_pop_2017",
                          "Population en sous-nutrition de 2016 à 2018" = "under_pop_2018",
                          "Population en sous-nutrition de 2017 à 2019" = "under_pop_2019",
                          "Disponibilité alimentaire journalière (kcl/j)" = "dispo_alim_kcalj",
                          "Disponibilité alimentaire mondial" = "dispo_alim_wor", 
                          
                          
                          )),
            tableOutput("data")),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    df <- reactive({
        if (input$choice == "pop_2014"){
        df <- population %>% filter(Year == 2014) %>% group_by(Area) %>% 
            summarise(sum = sum(Value)) %>% arrange(desc(sum)) %>% head(5) %>% 
            ggplot(aes(x = Area, y = sum)) + geom_bar(stat = 'identity')
        }
        if (input$choice == "pop_2015"){
            df <- population %>% filter(Year == 2015) %>% group_by(Area) %>% 
                summarise(sum = sum(Value)) %>% arrange(desc(sum)) %>% head(5) %>% 
                ggplot(aes(x = Area, y = sum)) + geom_bar(stat = 'identity')
        }
        if (input$choice == "pop_2016"){
            df <- population %>% filter(Year == 2016) %>% group_by(Area) %>% 
                summarise(sum = sum(Value)) %>% arrange(desc(sum)) %>% head(5) %>% 
                ggplot(aes(x = Area, y = sum)) + geom_bar(stat = 'identity')
        }
        if (input$choice == "pop_2017"){
            df <- population %>% filter(Year == 2017) %>% group_by(Area) %>% 
                summarise(sum = sum(Value)) %>% arrange(desc(sum)) %>% head(5) %>% 
                ggplot(aes(x = Area, y = sum)) + geom_bar(stat = 'identity')
        }
        if (input$choice == "pop_2018"){
            df <- population %>% filter(Year == 2018) %>% group_by(Area) %>% 
                summarise(sum = sum(Value)) %>% arrange(desc(sum)) %>% head(5) %>% 
                ggplot(aes(x = Area, y = sum)) + geom_bar(stat = 'identity')
        }
        if (input$choice == "under_pop_2019"){
            df <- under %>% group_by(Area) %>% filter(Year == '2017-2019') %>%
                summarise(sum = sum(Value)) %>% arrange(desc(sum)) %>% head(5) %>% 
                ggplot(aes(x = Area, y = sum)) + geom_bar(stat = 'identity')
        }
        if (input$choice == "under_pop_2018"){
            df <- under %>% group_by(Area) %>% filter(Year == '2016-2018') %>%
                summarise(sum = sum(Value)) %>% arrange(desc(sum)) %>% head(5) %>% 
                ggplot(aes(x = Area, y = sum)) + geom_bar(stat = 'identity')
        }
        if (input$choice == "under_pop_2017"){
            df <- under %>% group_by(Area) %>% filter(Year == '2015-2017') %>%
                summarise(sum = sum(Value)) %>% arrange(desc(sum)) %>% head(5) %>% 
                ggplot(aes(x = Area, y = sum)) + geom_bar(stat = 'identity')
        }
        if (input$choice == "dispo_alim_kcalj"){
            df <- dispo_alim %>% group_by(country) %>% filter(year == '2018') %>%
                summarise(sum = sum(dispo_alim_kcal_p_j)) %>% arrange(desc(sum)) %>% head(5) %>% 
                ggplot(aes(x = country, y = sum)) + geom_bar(stat = 'identity')
        }
        if (input$choice == "dispo_alim_wor"){
            df <- dispo_alim %>% group_by(country) %>% filter(year == '2018') %>%
                summarise(sum = sum(dispo_alim_tonnes)) %>% arrange(desc(sum)) %>% head(5) %>% 
                ggplot(aes(x = country, y = sum)) + geom_bar(stat = 'identity')
        }
        
        return(df)
    })
    output$plot <- renderPlot({df()})
}
# Run the application 
shinyApp(ui = ui, server = server)
