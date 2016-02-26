
#All floods, colors indicate flood type and size indicates magnitude
colorArray = matrix("", nrow = 6, ncol = 3)
colorArray[1,1] = "Heavy Rain"
colorArray[1,2] = "white"
colorArray[1,3] = 3/5
colorArray[2,1] = "Monsoon"
colorArray[2,2] = "#377eb8"
colorArray[2,3] = 3/4
colorArray[3,1] = "Hurricane/Tropical Storm"
colorArray[3,2] = "#ff7f00"
colorArray[3,3] = 3/5
colorArray[4,1] = "Snow/Ice Melt"
colorArray[4,2] = "#ffff33"
colorArray[4,3] = 5/6
colorArray[5,1] = "Dam Failure"
colorArray[5,2] = "#e41a1c"
colorArray[5,3] = 5/6
colorArray[6,1] = "Other"
colorArray[6,2] = "#4daf4a"
colorArray[6,3] = 1

cleanGlobalFloodData$numYearBegan = as.numeric(substr(
    as.character(cleanGlobalFloodData$dateBegan), 
    nchar(as.character(cleanGlobalFloodData$dateBegan))-3, 
    nchar(as.character(cleanGlobalFloodData$dateBegan))))
cleanGlobalFloodData$numMonthBegan = as.numeric(substr(
  as.character(cleanGlobalFloodData$dateBegan), 
  1, 
  regexpr("/",cleanGlobalFloodData$dateBegan)-1))
cleanGlobalFloodData$numYearEnded = as.numeric(substr(
  as.character(cleanGlobalFloodData$dateEnded), 
  nchar(as.character(cleanGlobalFloodData$dateEnded))-3, 
  nchar(as.character(cleanGlobalFloodData$dateEnded))))
cleanGlobalFloodData$numMonthEnded = as.numeric(substr(
  as.character(cleanGlobalFloodData$dateEnded), 
  1, 
  regexpr("/",cleanGlobalFloodData$dateEnded)-1))


library(animation)
saveHTML({
  ani.options(interval = 0.15)
  for (yr in 1985:2015) {
    for(mo in 1:12) {
      worldMap = ggplot() + 
        borders("world", colour = "gray30", fill="gray70") + 
        theme(panel.background = element_rect(fill = "gray80"),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_line(colour = "black")
        ) +
        scale_y_continuous(limits = c(-60,90)) +
        ggtitle(paste("Trailing 12mo Global Flood Magnitude in ", yr, sep="")) +
        
        geom_point(aes(x = subset(cleanGlobalFloodData, 
                                  (mainCause1 == colorArray[1,1] | 
                                     mainCause2 == colorArray[1,1] |
                                     mainCause3 == colorArray[1,1]) & 
                                    numYearBegan * 12 + numMonthBegan <= yr * 12 + mo &
                                    numYearEnded * 12 + numMonthEnded >= yr * 12 + mo - 12
        )$centroidX, 
        y = subset(cleanGlobalFloodData, 
                   (mainCause1 == colorArray[1,1] | 
                      mainCause2 == colorArray[1,1] |
                      mainCause3 == colorArray[1,1]) & 
                     numYearBegan * 12 + numMonthBegan <= yr * 12 + mo &
                     numYearEnded * 12 + numMonthEnded >= yr * 12 + mo - 12
        )$centroidY),
        color = colorArray[1,2], 
        size = subset(cleanGlobalFloodData, 
                      (mainCause1 == colorArray[1,1] | 
                         mainCause2 == colorArray[1,1] |
                         mainCause3 == colorArray[1,1]) & 
                        numYearBegan * 12 + numMonthBegan <= yr * 12 + mo &
                        numYearEnded * 12 + numMonthEnded >= yr * 12 + mo - 12
        )$magnitude^5/2000,
        alpha = colorArray[1,3]) + 
        geom_point(aes(x = subset(cleanGlobalFloodData, 
                                  (mainCause1 == colorArray[2,1] | 
                                     mainCause2 == colorArray[2,1] |
                                     mainCause3 == colorArray[2,1]) & 
                                    numYearBegan * 12 + numMonthBegan <= yr * 12 + mo &
                                    numYearEnded * 12 + numMonthEnded >= yr * 12 + mo - 12
        )$centroidX, 
        y = subset(cleanGlobalFloodData, 
                   (mainCause1 == colorArray[2,1] | 
                      mainCause2 == colorArray[2,1] |
                      mainCause3 == colorArray[2,1]) & 
                     numYearBegan * 12 + numMonthBegan <= yr * 12 + mo &
                     numYearEnded * 12 + numMonthEnded >= yr * 12 + mo - 12
        )$centroidY),
        color = colorArray[2,2], 
        size = subset(cleanGlobalFloodData, 
                      (mainCause1 == colorArray[2,1] | 
                         mainCause2 == colorArray[2,1] |
                         mainCause3 == colorArray[2,1]) & 
                        numYearBegan * 12 + numMonthBegan <= yr * 12 + mo &
                        numYearEnded * 12 + numMonthEnded >= yr * 12 + mo - 12
        )$magnitude^5/2000,
        alpha = colorArray[2,3])+ 
        geom_point(aes(x = subset(cleanGlobalFloodData, 
                                  (mainCause1 == colorArray[3,1] | 
                                     mainCause2 == colorArray[3,1] |
                                     mainCause3 == colorArray[3,1]) & 
                                    numYearBegan * 12 + numMonthBegan <= yr * 12 + mo &
                                    numYearEnded * 12 + numMonthEnded >= yr * 12 + mo - 12
        )$centroidX, 
        y = subset(cleanGlobalFloodData, 
                   (mainCause1 == colorArray[3,1] | 
                      mainCause2 == colorArray[3,1] |
                      mainCause3 == colorArray[3,1]) & 
                     numYearBegan * 12 + numMonthBegan <= yr * 12 + mo &
                     numYearEnded * 12 + numMonthEnded >= yr * 12 + mo - 12
        )$centroidY),
        color = colorArray[3,2], 
        size = subset(cleanGlobalFloodData, 
                      (mainCause1 == colorArray[3,1] | 
                         mainCause2 == colorArray[3,1] |
                         mainCause3 == colorArray[3,1]) & 
                        numYearBegan * 12 + numMonthBegan <= yr * 12 + mo &
                        numYearEnded * 12 + numMonthEnded >= yr * 12 + mo - 12
        )$magnitude^5/2000,
        alpha = colorArray[3,3])+ 
        geom_point(aes(x = subset(cleanGlobalFloodData, 
                                  (mainCause1 == colorArray[4,1] | 
                                     mainCause2 == colorArray[4,1] |
                                     mainCause3 == colorArray[4,1]) & 
                                    numYearBegan * 12 + numMonthBegan <= yr * 12 + mo &
                                    numYearEnded * 12 + numMonthEnded >= yr * 12 + mo - 12
        )$centroidX, 
        y = subset(cleanGlobalFloodData, 
                   (mainCause1 == colorArray[4,1] | 
                      mainCause2 == colorArray[4,1] |
                      mainCause3 == colorArray[4,1]) & 
                     numYearBegan * 12 + numMonthBegan <= yr * 12 + mo &
                     numYearEnded * 12 + numMonthEnded >= yr * 12 + mo - 12
        )$centroidY),
        color = colorArray[4,2], 
        size = subset(cleanGlobalFloodData, 
                      (mainCause1 == colorArray[4,1] | 
                         mainCause2 == colorArray[4,1] |
                         mainCause3 == colorArray[4,1]) & 
                        numYearBegan * 12 + numMonthBegan <= yr * 12 + mo &
                        numYearEnded * 12 + numMonthEnded >= yr * 12 + mo - 12
        )$magnitude^5/2000,
        alpha = colorArray[4,3])+ 
        geom_point(aes(x = subset(cleanGlobalFloodData, 
                                  (mainCause1 == colorArray[5,1] | 
                                     mainCause2 == colorArray[5,1] |
                                     mainCause3 == colorArray[5,1]) & 
                                    numYearBegan * 12 + numMonthBegan <= yr * 12 + mo &
                                    numYearEnded * 12 + numMonthEnded >= yr * 12 + mo - 12
        )$centroidX, 
        y = subset(cleanGlobalFloodData, 
                   (mainCause1 == colorArray[5,1] | 
                      mainCause2 == colorArray[5,1] |
                      mainCause3 == colorArray[5,1]) & 
                     numYearBegan * 12 + numMonthBegan <= yr * 12 + mo &
                     numYearEnded * 12 + numMonthEnded >= yr * 12 + mo - 12
        )$centroidY),
        color = colorArray[5,2], 
        size = subset(cleanGlobalFloodData, 
                      (mainCause1 == colorArray[5,1] | 
                         mainCause2 == colorArray[5,1] |
                         mainCause3 == colorArray[5,1]) & 
                        numYearBegan * 12 + numMonthBegan <= yr * 12 + mo &
                        numYearEnded * 12 + numMonthEnded >= yr * 12 + mo - 12
        )$magnitude^5/2000,
        alpha = colorArray[5,3])+ 
        geom_point(aes(x = subset(cleanGlobalFloodData, 
                                  !(mainCause1 %in% colorArray[,1] | 
                                      mainCause2 %in% colorArray[,1] |
                                      mainCause3 %in% colorArray[,1]) & 
                                    numYearBegan * 12 + numMonthBegan <= yr * 12 + mo &
                                    numYearEnded * 12 + numMonthEnded >= yr * 12 + mo - 12
        )$centroidX, 
        y = subset(cleanGlobalFloodData, 
                   !(mainCause1 %in% colorArray[,1] | 
                       mainCause2 %in% colorArray[,1] |
                       mainCause3 %in% colorArray[,1]) & 
                     numYearBegan * 12 + numMonthBegan <= yr * 12 + mo &
                     numYearEnded * 12 + numMonthEnded >= yr * 12 + mo - 12
        )$centroidY),
        color = colorArray[6,2], 
        size = subset(cleanGlobalFloodData, 
                      !(mainCause1 %in% colorArray[,1] | 
                          mainCause2 %in% colorArray[,1] |
                          mainCause3 %in% colorArray[,1]) & 
                        numYearBegan * 12 + numMonthBegan <= yr * 12 + mo &
                        numYearEnded * 12 + numMonthEnded >= yr * 12 + mo - 12
        )$magnitude^5/2000,
        alpha = colorArray[6,3])
      
      print(worldMap)
    }
  }
}, img.name = "magnitudePlots", imgdir = "magnitudePlots", htmlfile = "magnitudePlots.html", 
outdir = getwd(), autobrowse = FALSE, ani.height = 600, ani.width = 1100, 
verbose = FALSE, autoplay = TRUE, title = "Flood Magnitude through Time")

remove(mo)
remove(yr)

