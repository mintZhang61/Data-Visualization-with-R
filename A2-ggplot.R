library(ggplot2)

#task1. read data
coral = read.csv("assignment-02-formatted-data.csv", 
                 sep = ",", 
                 header = TRUE)

dff <- within(coral, {bleaching <- as.numeric(sub('%', "", as.character(bleaching)))/100})

dff$year = dff$ï..year

#reorder sites
dff$site <- factor(dff$site, 
                   levels = c("site06","site01", "site05", "site02", 
                              "site08", "site03", "site07", "site04" ))
#task2. plot
ggplot(data = dff, aes(x = year, y = bleaching), ) +
  geom_point(size = 2, shape = 1) +
  facet_grid(site ~ type, scales =  'free') +
  geom_smooth(method = 'lm', color = "orange") +
  labs(x = "\nYear", y = "Degree of bleaching", 
       title = "Each site how bleaching caries from year to year(Order by latitude)")


