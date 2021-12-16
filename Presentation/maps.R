library(shiny)
library(ggplot2)
library(stringr)
library(DT)
library(dplyr)
library(readxl)
# Load data --------------------------------------------------------------------
MI_2012 <- read_excel("~/STA_518/Preparations/Presentation/data/MI_CountyLevelSummary_2012.xlsx")
MI_2013 <- read_excel("~/STA_518/Preparations/Presentation/data/MI_CountyLevelSummary_2013.xlsx")
MI_2014 <- read_excel("~/STA_518/Preparations/Presentation/data/MI_CountyLevelSummary_2014.xlsx")
MI_2015 <- read_excel("~/STA_518/Preparations/Presentation/data/MI_CountyLevelSummary_2015.xlsx")
MI_2016 <- read_excel("~/STA_518/Preparations/Presentation/data/MI_CountyLevelSummary_2016.xlsx")
Total <- bind_rows(MI_2012, MI_2013, MI_2014, MI_2015, MI_2016)
Total <- Total %>% 
    mutate(proportion <-   Confirmed/Tested) %>% 
    mutate(percent =  100*Proportion) %>% 
    mutate(percent = round(percent,3)) %>% 
    mutate(subregion=tolower(str_replace(subregion, " County",""))) %>% 
    mutate(County <- str_to_title(subregion))
usa <- map_data('usa')
state <- map_data("state")
michigan <- subset(state, region=="michigan")
counties <- map_data("county")
michigan_county <- subset(counties, region=="michigan")

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "Year", 
                        label = "Select the year you would like to look at:",
                        choices = c("2012", "2013", "2014", "2015", "2016"), 
                        selected = "2016"),
            
            checkboxInput(inputId = "show_data",
                          label = "Show county data table", 
                          value = TRUE),
            dataTableOutput(outputId = "countytable", width = "80%")
        ),
        
    
    
    
    mainPanel(
            plotOutput(outputId = "choropleth", width = "100%")

            )
        )
)


# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
    
    output$countytable <- renderDataTable({
    if(input$show_data){
            table <- select(Total, 2,16, 18)
            table <- filter(table,Year==input$Year)
          table2 <-  select(table, 1,3)
          table2 %>%
            arrange(desc(Proportion)) %>%
            mutate(subregion= str_to_title(subregion)) %>% 
            datatable(head(table2), colnames = c('Subregion'='subregion', 'Percent'='Proportion'),
                      options= list(lengthMenu=c(5,10,25)))%>% 
            formatPercentage(columns='Percent', digits=2) 

            
    }

        })
    
    output$choropleth <- renderPlot({
        ca_map <- ggplot(michigan, mapping=aes(x=long, y=lat, group=group)) + 
            coord_fixed(1.3) + 
            geom_polygon(color="black", fill="grey") + 
            geom_polygon(data= michigan_county, fill=NA, color="blue") + 
            geom_polygon(color="black", fill=NA) + 
            ggtitle('Michigan Map with Counties') + 
            theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
                  axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
        ca_map
        map <- Total %>% 
            mutate(subregion=tolower(str_replace(subregion, " County","")))
        michigan_county2 <- michigan_county %>%
            left_join(Total, by = "subregion")
        selected_year <- michigan_county2 %>% 
            filter(Year==input$Year)
        plot <- ggplot(selected_year, aes(long, lat, group=group, fill = Proportion)) +
            geom_polygon() + 
            coord_quickmap() 
        plot
    })
    
}

# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)


