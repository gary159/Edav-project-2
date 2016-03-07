library(ggplot2)
library(maps)

#Read in cleanGlobalFloodData
#cleanGlobalFloodData$dateBegan = as.Date.factor(cleanGlobalFloodData$dateBegan, "%m/%d/%Y")
#cleanGlobalFloodData$dateEnded = as.Date.factor(cleanGlobalFloodData$dateEnded, "%m/%d/%Y")

causes = c("Heavy Rain", "Monsoon", "Hurricane/Tropical Storm", 
           "Snow/Ice Melt", "Dam Failure")

causeColors = c("white", "#ffff33", "#ff7f00", "#756bb1", "#e41a1c",
                "#4daf4a") #Other color

globalFloods = 
  cbind(subset(cleanGlobalFloodData, 
                 !(mainCause1 %in% causes[] & 
                     mainCause2 %in% causes[] &
                     mainCause3 %in% causes[]))
        [, c("centroidX", "centroidY", "magnitude", "dateBegan", "dateEnded")],
        FloodType = "Other")

for(x in 1:length(causes)){
  globalFloods = rbind(globalFloods, 
                       cbind(subset(cleanGlobalFloodData, 
                                      (mainCause1 == causes[x] | 
                                         mainCause2 == causes[x] |
                                         mainCause3 == causes[x]))
                             [, c("centroidX", "centroidY", "magnitude", "dateBegan", "dateEnded")],
                             FloodType = causes[x]
                       )                 
  )
  
}  


ggplot() +
  borders("world", colour = "gray10", fill="gray40") + 
  theme(panel.background = element_rect(fill = "gray80"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "black")
  ) + 
  coord_cartesian(xlim = c(-180, 180), ylim=c(-60, 90)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  
  geom_point(data = globalFloods, 
             aes(x = centroidX, y = centroidY, color = FloodType, size = magnitude),
             alpha = 0.3) +
  scale_color_manual(breaks = append(causes, "Other"), 
                     limits = append(causes, "Other"), 
                     values = causeColors) +
  scale_size(trans = "exp", range=c(0, 20), limits = c(0,9), breaks=c(4, 5, 6, 7, 8)) + 
  guides(color = guide_legend(override.aes = list(size = 10), 
                              title = "Flood Cause"),
         size = guide_legend(title = "Flood Magnitude")) 

remove(causes)
remove(causeColors)
remove(x)
