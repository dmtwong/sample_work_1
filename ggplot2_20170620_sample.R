# ggplot 4x1 NtT code stop at 128; input '20170620_currency_53_2.csv' 
# output: create folder 'Plots' in your working directory to store plots


rm(list= ls())
#setwd('C:\\Users\\dmtwong\\Desktop')
setwd('C:\\Users\\david\\Desktop')
install.packages(c('lubridate','tidyr','ggplot2', 'gridExtra'))
require(lubridate); require(tidyr); require(ggplot2); require(grid); require(gridExtra)
require(reshape2); require(gtable); require(scales); require(stringr)

install.packages('zoo')
library(zoo)
###############################################################################

#currency   <- read.csv("20170620_currency_53.csv")
currency   <- read.csv("20170620_currency_53_2.csv") # all NA are replaced by 0
#str(currency)
tmp <- levels(currency$countries) 
#currency$countries[790:810]
levels(currency$countries) <- c(tmp[1: 9], 'CHINA', 'HONG KONG', tmp[12:53])

curr_crisis <- currency[, c(2,1,3)] #3 as MIPRCrisis
# str(curr_crisis)
GlobalNtP   <- currency[, c(2,1,4)];GlobalNtT   <- currency[, c(2,1,5)]
RegionalNtP <- currency[, c(2,1,6)];RegionalNtT <- currency[, c(2,1,7)]
CountryNtP  <- currency[, c(2,1,9)];CountryNtT  <- currency[, c(2,1,10)]
#equity_crisis$quarter2 <- as.yearqtr(currency_crisis$quarter2)
my_currency <- spread(curr_crisis, countries, MIPRCrisis, drop = FALSE)
#str(my_currency); my_currency[c('quarter2','GREECE')]
GlobalNtP   <- spread(GlobalNtP, countries, GlobalNtP, drop = FALSE)
GlobalNtT   <- spread(GlobalNtT, countries, GlobalNtT, drop = FALSE)
RegionalNtP <- spread(RegionalNtP, countries, RegionalNtP, drop = FALSE)
RegionalNtT <- spread(RegionalNtT, countries, RegionalNtT, drop = FALSE)
CountryNtP  <- spread(CountryNtP, countries, CountryNtP, drop = FALSE)
CountryNtT  <- spread(CountryNtT, countries, CountryNtT, drop = FALSE)

#equity <- read.csv("20170620_equity_53.csv")
equity <- read.csv("20170620_equity_53_2.csv") #same: NA replace by 0 
#str(equity)
tmp2 <- levels(equity$country) 
#equity$country[790:810]
levels(equity$country) <- c(tmp2[1: 9], 'CHINA', 'HONG KONG', tmp2[12:53])
  
#str(equity)
eq_crisis <- equity[, c(2,1,3)] #3 as crisisST
my_equity <- spread(eq_crisis, country, crisisST, drop = FALSE)
# str(my_equity); my_equity[c('quarter2','GREECE')]

countries <- colnames(my_currency)[-1]
#countries_2 <- colnames(my_equity)[-1]
#countries %in% countries_2

#**********Creating plots (4 times 1 plot )********** -----------
# Note1: uncomment NtT/NtP and comment out the other to produce another set
# of graphs
# Note2: L71 to L73 replace by Ntp/NtT 

for (i in countries) {
  prep_df <- data.frame(my_equity$quarter2, my_equity[,i], my_currency[,i], 
                        #GlobalNtP[,i], RegionalNtP[,i], CountryNtP[,i],
                        GlobalNtT[,i], RegionalNtT[,i], CountryNtT[,i])
  colnames(prep_df) <- c("Quarter", "Equity Crisis", "Currency Crisis",
                         #"Glob NtP", "Region NtP", "Country NtP",
                         "Glob NtT", "Region NtT", "Country NtT")
  prep_df$Quarter <- as.Date( as.yearqtr(prep_df$Quarter) )
  long_prep_df <- gather(prep_df, Key, Values, -Quarter)
  my_df <- long_prep_df
  my_df$Key2 <- as.character(my_df$Key)
  #########  For Ntp, replace
  #my_df$Key2[my_df$Key2 %in% c("Glob NtP", "Region NtP", "Country NtP")] <- "Neg-to-Pos"
  #########
  my_df$Key2[my_df$Key2 %in% "Glob NtT"] <- "Global Neg-to-Tot"
  my_df$Key2[my_df$Key2 %in% "Region NtT"] <- "Regional Neg-to-Tot"
  my_df$Key2[my_df$Key2 %in% "Country NtT"] <- "Country Neg-to-Tot"
  
  my_df$Key2[my_df$Key2 %in% c("Equity Crisis", "Currency Crisis")] <- "Crisis"
  
  my_df[my_df == Inf] <- 0  

  png(filename = paste0("Plots/", i, ".png"), width = 600, height = 600)
  #Now begins the plotting
  plot_top <- ggplot() + scale_x_date(breaks = date_breaks("1 year")) + #scale_y_discrete(breaks=c(-0.1,0.0,1.0,1.1), labels=c(NULL,0.0,1.0,NULL)) +
    geom_segment(data=subset(my_df, Key2=="Crisis"), 
                 mapping=aes(Quarter, Values, yend=0, xend=Quarter, colour=Key)) +
    theme_bw() +  scale_colour_manual(values = c("orange1", "gray20")) + 
    labs(list(title = i, x = "", y = "", colour="Crisis Dummy")) +
    theme(plot.title = element_text(size = 16, face = "bold", vjust = 1), 
          axis.ticks.x=element_blank(), axis.text.x=element_blank(), 
          plot.margin=unit(c(0.25,0.1,-0.5,0.1), "lines"))
  
  plot_2 <- ggplot() + geom_segment(data=subset(my_df, Key2=="Country Neg-to-Tot"), 
                                      mapping=aes(Quarter, Values, yend=0, xend=Quarter, colour=Key)) +
    labs(list(x = "", y = "", color="Country Neg-to-Tot")) + scale_x_date(breaks = date_breaks("1 year")) +
    theme_bw() + theme(axis.ticks.x=element_blank(), axis.text.x=element_blank(), 
                       plot.margin=unit(c(-0.5,0.1,-0.5,0.1), "lines"))
  plot_3 <- ggplot() + geom_segment(data=subset(my_df, Key2=="Regional Neg-to-Tot"), 
                                    mapping=aes(Quarter, Values, yend=0, xend=Quarter, colour=Key)) +
    labs(list(x = "", y = "", color="Regional Neg-to-Tot")) + scale_x_date(breaks = date_breaks("1 year")) +
    theme_bw() + theme(axis.ticks.x=element_blank(), axis.text.x=element_blank(), 
                       plot.margin=unit(c(-0.5,0.1,-0.5,0.1), "lines"))
  
  plot_bot <- ggplot() +  geom_segment(data=subset(my_df, Key2=="Global Neg-to-Tot"), 
                                       mapping=aes(Quarter, Values, yend=0, xend=Quarter, colour=Key)) + theme_bw() +
    labs(list(y = "", colour="Global Neg-to-Tot")) + theme(plot.margin=unit(c(-0.5,0.1,-0.5,0.1), "lines"),
                                                    axis.text.x=element_text(angle = 45, hjust=1)) +
    scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y"))
  #  
  #The following lines prior to grid.arrange align the plots (despite differences in ylabel lengths)
  gTop <- ggplotGrob(plot_top)
  g2 <- ggplotGrob(plot_2)
  g3 <- ggplotGrob(plot_3)
  gBot <- ggplotGrob(plot_bot)
  grid::grid.newpage()
  grid::grid.draw(rbind(gTop,g2,g3,gBot))
  
  
  
  #TEMP# maxWidth = grid::unit.pmax(gTop$widths[2:6], gMid$widths[2:6], gBot$widths[2:6])
  #gTop$widths[2:5] <- as.list(maxWidth)
  #gMid$widths[2:5] <- as.list(maxWidth)
  #gBot$widths[2:5] <- as.list(maxWidth)
  #TEMP#gTop$widths[2:6] <- maxWidth
  #TEMP#gMid$widths[2:6] <- maxWidth
  #TEMP#gBot$widths[2:6] <- maxWidth
  
  #grid.arrange(gTop, gMid, gBot, nrow=3, ncol=1, heights=c(0.35,0.30,0.35))
  
  dev.off()
}
save.image('plot_NtT_170620.RData')
####################4 * 1 plot end ###############################

#######################Archived ###########################################

#**********Creating plots (Fixed Default but now archived  )********** -----------

#for (i in countries) {
  #print(i)
  #if (i == countries[10]){
    #print( my_currency[, i] ); print( my_equity[, i] ); #print( GlobalNtP[, i] )
    #print( GlobalNtT[, i] ); print( RegionalNtP[, i] ); print( CountryNtP[, i] )
    #print( CountryNtT[, i] )  }
  #prep_df <- data.frame(my_equity$quarter2, my_equity[,i], my_currency[,i], 
    #                    GlobalNtP[,i], GlobalNtT[,i], RegionalNtP[,i], RegionalNtT[,i], 
     #                   CountryNtP[,i], CountryNtT[,i])
  #colnames(prep_df) <- c("Quarter", "E Crisis", "Crncy Crisis","Glob NtP", "Glob NtT", 
  #colnames(prep_df) <- c("Quarter", "Equity Crisis", "Currency Crisis","Glob NtP", "Glob NtT", 
   #                      "Region NtP", "Region NtT", "Country NtP", "Country NtT")
  # prep_df$Quarter <- as.Date(prep_df$Date)
  # prep_df$Quarter <- as.yearqtr(prep_df$Quarter)# Invalid input: date_trans works with objects of class Date only
  #prep_df$Quarter <- as.Date( as.yearqtr(prep_df$Quarter) )
  #long_prep_df <- gather(prep_df, Key, Values, -Quarter)
  #my_df <- long_prep_df
  #print( head(my_df) )
  #my_df$Key2 <- as.character(my_df$Key)
  #print( my_df$Key2 )
  #my_df$Key2[my_df$Key2 %in% c("Glob NtP", "Region NtP", "Country NtP")] <- "Neg-to-Pos"
  #my_df$Key2[my_df$Key2 %in% c("Glob NtT", "Region NtT", "Country NtT")] <- "Neg-to-Tot"
  #my_df$Key2[my_df$Key2 %in% c("Equity Crisis", "Currency Crisis")] <- "Crisis"
  #my_df$Key2[my_df$Key2 %in% c("E Crisis", "Crncy Crisis")] <- "Crisis"
  
  #print( head(my_df) ); print( str(my_df) )
  #my_df[my_df == Inf] <- 0
  
  ## #Now begins the plotting, adjust location for saving and plot sizes in the following line
  #png(filename = paste0("Plots/", i, ".png"), width = 600, height = 600)
  #plot_top <- ggplot() + scale_x_date(breaks = date_breaks("1 year")) + #scale_y_discrete(breaks=c(-0.1,0.0,1.0,1.1), labels=c(NULL,0.0,1.0,NULL)) +
   # geom_segment(data=subset(my_df, Key2=="Crisis"), 
    #             mapping=aes(Quarter, Values, yend=0, xend=Quarter, colour=Key)) +
    # theme_bw() +  scale_colour_manual(values = c("gray20", "orange1")) + 
    #theme_bw() +  scale_colour_manual(values = c("orange1", "gray20")) + 
    #labs(list(title = i, x = "", y = "", colour="Crisis Dum")) +
    #theme(plot.title = element_text(size = 16, face = "bold", vjust = 1), 
     #     axis.ticks.x=element_blank(), axis.text.x=element_blank(), 
      #    plot.margin=unit(c(0.25,0.1,-0.5,0.1), "lines"))
  
  #plot_mid <- ggplot() + geom_segment(data=subset(my_df, Key2=="Neg-to-Tot"), 
   #                                   mapping=aes(Quarter, Values, yend=0, xend=Quarter, colour=Key)) +
    #labs(list(x = "", y = "", color="Neg-to-Tot")) + scale_x_date(breaks = date_breaks("1 year")) +
    #theme_bw() + theme(axis.ticks.x=element_blank(), axis.text.x=element_blank(), 
     #                  plot.margin=unit(c(-0.5,0.1,-0.5,0.1), "lines"))
  
  #plot_bot <- ggplot() +  geom_segment(data=subset(my_df, Key2=="Neg-to-Pos"), 
  #                                     mapping=aes(Quarter, Values, yend=0, xend=Quarter, colour=Key)) + theme_bw() +
   # labs(list(y = "", colour="Neg-to-Pos")) + theme(plot.margin=unit(c(-0.5,0.1,-0.5,0.1), "lines"),
    #                                                axis.text.x=element_text(angle = 45, hjust=1)) +
    #scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y"))
#  
  #The following lines prior to grid.arrange align the plots (despite differences in ylabel lengths)
  #gTop <- ggplotGrob(plot_top)
  #gMid <- ggplotGrob(plot_mid)
  #gBot <- ggplotGrob(plot_bot)
  #grid::grid.newpage()
  #grid::grid.draw(rbind(gTop,gMid,gBot))
  
  
  
  #TEMP# maxWidth = grid::unit.pmax(gTop$widths[2:6], gMid$widths[2:6], gBot$widths[2:6])
  #gTop$widths[2:5] <- as.list(maxWidth)
  #gMid$widths[2:5] <- as.list(maxWidth)
  #gBot$widths[2:5] <- as.list(maxWidth)
  #TEMP#gTop$widths[2:6] <- maxWidth
  #TEMP#gMid$widths[2:6] <- maxWidth
  #TEMP#gBot$widths[2:6] <- maxWidth
  
  #grid.arrange(gTop, gMid, gBot, nrow=3, ncol=1, heights=c(0.35,0.30,0.35))
  
  #dev.off()
#}

###################Deep Playground##################################
#colnames(my_currency)[ colnames(my_currency) %in% colnames(my_equity) ]

#colnames(my_currency)[ ! colnames(my_currency) %in% colnames(my_equity) ]
# Countries in currency_61 but not in equity
# [1] "ISRAEL"          "LEBANON"         "NIGERIA"         "SAUDI ARABIA"    "SLOVAK REPUBLIC"
# [6] "SOUTH AFRICA"    "TANZANIA"        "TURKEY"  
#colnames(my_equity)[ ! colnames(my_equity) %in% colnames(my_currency) ]
# [1] "BERMUDA"              "BOSNIA & HERZEGOVINA" "COSTA RICA"           "CYPRUS"               "ESTONIA"             
#[6] "JAMAICA"              "KAZAKHSTAN"           "LAO PEOPLE'S DEM.REP" "LATVIA"               "LUXEMBOURG"          
#[11] "MACEDONIA, FYR"       "MALTA"                "MONGOLIA"             "MONTENEGRO"           "PANAMA"              
#[16] "SERBIA, REPUBLIC OF" 

#currency_61_data <- read.csv("20161017 CrncyCrisis 61Cnty Qtrly.csv")
# names(table(currency_61_data$quarter2))[ names(table(currency_61_data$quarter2)) >= "1995q1" ]
#currency_61_data <- currency_61_data[labels(currency_61_data$quarter2) > '1994Q4',]
#str(currency_61_data)
#currency_crisis <- currency_61_data[, c(2,1,7)] #7 as MIPRCrisis (Policy rate)
#str(currency_crisis)
#currency_crisis$quarter2 <- as.yearqtr(currency_crisis$quarter2)
#str(currency_61_data)
#my_currency <- spread(currency_crisis, countries, MIPRCrisis, drop = FALSE)
#str(my_currency)







