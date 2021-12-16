# Load packages ----------------------------------------------------------------

library(shiny)
library(dplyr)
library(stringr)
library(readxl)
library(ggplot2)
# Load data --------------------------------------------------------------------
MI_2012 <- read_excel("data/MI_CountyLevelSummary_2012.xlsx")
MI_2013 <- read_excel("data/MI_CountyLevelSummary_2013.xlsx")
MI_2014 <- read_excel("data/MI_CountyLevelSummary_2014.xlsx")
MI_2015 <- read_excel("data/MI_CountyLevelSummary_2015.xlsx")
MI_2016 <- read_excel("data/MI_CountyLevelSummary_2016.xlsx")
#combining all year data into one data set called Total
Total <- bind_rows(MI_2012, MI_2013, MI_2014, MI_2015, MI_2016)
Total <- Total %>%
    mutate(state = "Michigan") %>%
    mutate(county <-  subregion ) %>%
    mutate(proportion <-   Confirmed/Tested) %>%
    mutate(percent =  100*Proportion) %>%
    mutate(percent = round(percent,3)) %>%
    mutate(subregion=tolower(str_replace(subregion, " County",""))) %>% 
    mutate(capsubregion1= str_to_title(subregion)) %>% 
    mutate(capsubregion2= str_to_title(subregion))


# Define UI --------------------------------------------------------------------

ui <- fluidPage(
    
        
        mainPanel(
            
            textInput(inputId = "subregion1", 
                        label = "Input county of interest:",
                        value= "Kent"),
                      
            textInput(inputId = "subregion2", 
                      label = "Input county of interest:",
                      value= "Ottawa"),
        
            plotOutput(outputId = "linegraph")
        )
    )

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
    output$linegraph <- renderPlot({
                a <- subset(Total,capsubregion1==input$subregion1) 
                b <- subset(Total,capsubregion2==input$subregion2) 
                ggplot() +
                    geom_line(a,mapping=aes(x=Year, y=Proportion), color="red") +
                    geom_line(b,mapping=aes(x=Year, y=Proportion), color="blue")+
                    scale_color_manual(values=c(a="red",b="blue"))

        
    })
    
}

# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)
