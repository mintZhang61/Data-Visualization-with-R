library(shiny)
library(ggplot2)
library(datasets)
library(leaflet)
#task3,5 shiny
coral <- read.csv("assignment-02-formatted-data.csv", 
                  sep = ",", 
                  header = TRUE)


# dataframe for the ggplot
df <- within(coral, {bleaching <- as.numeric(sub('%', "", as.character(bleaching)))/100})
df$year <- df$ï..year
df <- as.data.frame(df)


#dataframe for the map
df2 <- data.frame(cbind(long = c(unique(df$longitude)),
                        lat = c(unique(df$latitude)),
                        site = c(1:8)
))
# icon of map
df2$site <- iconv(df2$site)

#UI
ui <- fluidPage(
  headerPanel("Coral Bleaching"),
  
  pageWithSidebar(
    
    titlePanel("The change with Different coral in different sites"),
    
    sidebarLayout(
      sidebarPanel(
        # species selections
        selectInput("Species", "Please select a species of coral",
                    choices = c("blue corals", "hard corals", "sea pens",
                                "sea fans", "soft corals")
        ),
      
        # smoothers selections
        selectInput( "Smoothers", 
                     "Please select a level of smoothers",
                     choices = c("lm", "glm", "gam", "loess", "rlm")
                     )
        ),
    
      mainPanel(
        leafletOutput("map"),
        textOutput("caption"),
      
        plotOutput("myplot"),
        
      ),
      
      position = c("left", "right"),
      fluid = TRUE
      )
  )
  
)
#server
server <- function(input, output){
  output$caption <- renderText({
    input$caption
  })
  
  #map
  output$map <- renderLeaflet({
    ma <- leaflet(df2) %>%
      addTiles() %>%
      #addMarkers(~long, ~lat, popup = ~site)
      addCircles(lng = ~long, lat =~lat, weight = 10,
                 color = "red",
                 popup = ~site,
                 label = ~site)
    })
  
  # plot
  output$myplot <- renderPlot({
    if (input$Species == "blue corals"){
      mydat <- subset(df, type == "blue corals")
      
    }
    if (input$Species == "hard corals"){
      mydat <- subset(df, type == "hard corals")
      
    }
    if (input$Species == "sea pens"){
      mydat <- subset(df, type == "sea pens")
      
    }
    if (input$Species == "soft corals"){
      mydat <- subset(df, type == "soft corals")
      
    }
    if (input$Species == "sea fans"){
      mydat <- subset(df, type == "sea fans")
      
    }
    p = ggplot(mydat, aes(x = year, y = bleaching))
    p = p + geom_point(data = mydat, 
                       aes(x = year, y = bleaching),
                       size = 3 
                       ) +
      facet_grid(site ~ type, scales =  'free_y') +
      
      labs(x = "\nYear", y = "Degree of bleaching", 
           title = "Each site how bleaching caries from year to year")+
      theme(axis.title.x = element_text(size = 15), 
            axis.title.y = element_text(size = 15),
            axis.title = element_text(size = 15),
            legend.title = element_text(size = 10)
            #legend.title = "Each site how bleaching caries from year to year"
            )
    #"lm", "glm", "gam", "loess", "rlm")
    
    if(input$Smoothers == 'lm'){
      
      p = p + geom_smooth(method = 'lm', color = "green") 
      
    }
    if(input$Smoothers == "glm"){
      p = p + geom_smooth(method = 'glm', color = "green")
    }
    if(input$Smoothers == "gam"){
      p = p + geom_smooth(method = 'gam', color = "green")
    }
    if(input$Smoothers == "loess"){
      p = p + geom_smooth(method = 'loess', color = "green")
    }
    if(input$Smoothers == "rlm"){
      p  = p + geom_smooth(method = 'rlm', color = "green")
    }
    
    suppressWarnings(print(p))
  }
  )
  
}

shinyApp(ui = ui, server = server)
