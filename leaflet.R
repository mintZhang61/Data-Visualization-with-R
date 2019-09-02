library(leaflet)
#task3. leaflet
coral = read.csv("assignment-02-formatted-data.csv", 
                 sep = ",", 
                 header = TRUE)

#create dateframe for storage of address
df2 <- data.frame(cbind(long = c(as.numeric(unique(coral$longitude))),
                  lat = c(as.numberic(unique(coral$latitude))),
                  site = c(1:8)
                  ))


df2$site <- iconv(df2$site)

ma <- leaflet(df2) %>%
  addTiles() %>%
  addCircles( lng = ~long, lat =~lat, weight = 10,
              color = "grey",
              label = ~site)
ma

