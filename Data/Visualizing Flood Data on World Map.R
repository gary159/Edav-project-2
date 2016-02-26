library(ggplot2)
library(maps)

circleSize = 1/2000

#All floods, colors indicate flood type and size indicates magnitude
colorArray = matrix("", nrow = 6, ncol = 3)
colorArray[1,1] = "Heavy Rain"
colorArray[1,2] = "white"
colorArray[1,3] = 1/5
colorArray[2,1] = "Monsoon"
colorArray[2,2] = "#377eb8"
colorArray[2,3] = 1/4
colorArray[3,1] = "Hurricane/Tropical Storm"
colorArray[3,2] = "#ff7f00"
colorArray[3,3] = 1/5
colorArray[4,1] = "Snow/Ice Melt"
colorArray[4,2] = "#ffff33"
colorArray[4,3] = 1/2
colorArray[5,1] = "Dam Failure"
colorArray[5,2] = "#e41a1c"
colorArray[5,3] = 1/2
colorArray[6,1] = "Other"
colorArray[6,2] = "#4daf4a"
colorArray[6,3] = 4/5

worldMap = ggplot() + 
  borders("world", colour = "gray10", fill="gray40") + 
  theme(panel.background = element_rect(fill = "gray80"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "black"),
        legend.title = element_text("my legend")
        ) +
  scale_y_continuous(limits = c(-60,90)) +
  
  geom_point(aes(x = subset(cleanGlobalFloodData, 
                            mainCause1 == colorArray[1,1] | 
                              mainCause2 == colorArray[1,1] |
                              mainCause3 == colorArray[1,1]
                            )$centroidX, 
                 y = subset(cleanGlobalFloodData, 
                            mainCause1 == colorArray[1,1] | 
                              mainCause2 == colorArray[1,1] |
                              mainCause3 == colorArray[1,1]
                            )$centroidY),
             color = colorArray[1,2], 
             size = subset(cleanGlobalFloodData, 
                           mainCause1 == colorArray[1,1] | 
                             mainCause2 == colorArray[1,1] |
                             mainCause3 == colorArray[1,1]
             )$magnitude^5*circleSize,
             alpha = colorArray[1,3]) + 
  geom_point(aes(x = subset(cleanGlobalFloodData, 
                            mainCause1 == colorArray[2,1] | 
                              mainCause2 == colorArray[2,1] |
                              mainCause3 == colorArray[2,1]
                          )$centroidX, 
                y = subset(cleanGlobalFloodData, 
                           mainCause1 == colorArray[2,1] | 
                             mainCause2 == colorArray[2,1] |
                             mainCause3 == colorArray[2,1]
                          )$centroidY),
                color = colorArray[2,2], 
                size = subset(cleanGlobalFloodData, 
                              mainCause1 == colorArray[2,1] | 
                                mainCause2 == colorArray[2,1] |
                                mainCause3 == colorArray[2,1]
                          )$magnitude^5*circleSize,
                alpha = colorArray[2,3])+ 
  geom_point(aes(x = subset(cleanGlobalFloodData, 
                            mainCause1 == colorArray[3,1] | 
                              mainCause2 == colorArray[3,1] |
                              mainCause3 == colorArray[3,1]
                )$centroidX, 
                y = subset(cleanGlobalFloodData, 
                           mainCause1 == colorArray[3,1] | 
                             mainCause2 == colorArray[3,1] |
                             mainCause3 == colorArray[3,1]
                )$centroidY),
                color = colorArray[3,2], 
                size = subset(cleanGlobalFloodData, 
                              mainCause1 == colorArray[3,1] | 
                                mainCause2 == colorArray[3,1] |
                                mainCause3 == colorArray[3,1]
                )$magnitude^5*circleSize,
                alpha = colorArray[3,3])+ 
  geom_point(aes(x = subset(cleanGlobalFloodData, 
                            mainCause1 == colorArray[4,1] | 
                              mainCause2 == colorArray[4,1] |
                              mainCause3 == colorArray[4,1]
                )$centroidX, 
                y = subset(cleanGlobalFloodData, 
                           mainCause1 == colorArray[4,1] | 
                             mainCause2 == colorArray[4,1] |
                             mainCause3 == colorArray[4,1]
                )$centroidY),
                color = colorArray[4,2], 
                size = subset(cleanGlobalFloodData, 
                              mainCause1 == colorArray[4,1] | 
                                mainCause2 == colorArray[4,1] |
                                mainCause3 == colorArray[4,1]
                )$magnitude^5*circleSize,
                alpha = colorArray[4,3])+ 
  geom_point(aes(x = subset(cleanGlobalFloodData, 
                            mainCause1 == colorArray[5,1] | 
                              mainCause2 == colorArray[5,1] |
                              mainCause3 == colorArray[5,1]
                )$centroidX, 
                y = subset(cleanGlobalFloodData, 
                           mainCause1 == colorArray[5,1] | 
                             mainCause2 == colorArray[5,1] |
                             mainCause3 == colorArray[5,1]
                )$centroidY),
                color = colorArray[5,2], 
                size = subset(cleanGlobalFloodData, 
                              mainCause1 == colorArray[5,1] | 
                                mainCause2 == colorArray[5,1] |
                                mainCause3 == colorArray[5,1]
                )$magnitude^5*circleSize,
                alpha = colorArray[5,3])+ 
  geom_point(aes(x = subset(cleanGlobalFloodData, 
                            !(mainCause1 %in% colorArray[,1] | 
                              mainCause2 %in% colorArray[,1] |
                              mainCause3 %in% colorArray[,1])
                )$centroidX, 
                y = subset(cleanGlobalFloodData, 
                           !(mainCause1 %in% colorArray[,1] | 
                               mainCause2 %in% colorArray[,1] |
                               mainCause3 %in% colorArray[,1])
                )$centroidY),
                color = colorArray[6,2], 
                size = subset(cleanGlobalFloodData, 
                              !(mainCause1 %in% colorArray[,1] | 
                                  mainCause2 %in% colorArray[,1] |
                                  mainCause3 %in% colorArray[,1])
                )$magnitude^5*circleSize,
                alpha = colorArray[6,3])
  


worldMap

remove(circleSize)


