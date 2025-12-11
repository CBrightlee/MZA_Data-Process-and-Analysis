##author: Christian Bright
##date: 11/21/2024
##purpose: basic flux processing and relationships

library(corrplot)
library(ggplot2)
library(dplyr)
library(tidyr)
library(clock)
library(tidyverse)
library(lubridate)

##Brings in data from Montezuma and cleans it up

######### 2024 DATA ##########
setwd("C:/MontezumaData/2024_Data/EC_Data/EddyProOutputs/Aug2025_Rerun2")
EP_MWR <- read.csv("eddypro_MZA2024_fluxnet_2025-08-05T135405_adv.csv", header=TRUE)

######### 2025 DATA #########
# setwd("C:/MontezumaData/2025_Data/EC_Data/EddyProOutput/")
# EP_MWR1<-read.csv("eddypro_MZA2025_Pt1_fluxnet_2025-08-29T142848_adv.csv", header=TRUE)
# EP_MWR2 <- read.csv("eddypro_MZA2025_Pt2_fluxnet_2025-08-29T150430_adv.csv", header=TRUE)
# EP_MWR <- rbind(EP_MWR1, EP_MWR2)

#Low turbulence filtering for CH4
EP_MWR$FCH4[EP_MWR$USTAR<=0.15]<-NA        #Ustar filter
EP_MWR$FCH4[EP_MWR$FCH4 == -9999] <- NA    #Converts -9999s
EP_MWR$FCH4[EP_MWR$FCH4_SS_TEST>=7] <- NA #Filters out data that could be NG from no turbulence
#EP_MWR$FCH4[EP_MWR$FCH4 > 7*10^7]<-NA     #Weird outlier in 2025 being killed by this

# Standard deviation filtering for CH4
sd_CH4_threshold <- sd(na.omit(EP_MWR$FCH4))*6
mean_CH4 <- mean(na.omit(EP_MWR$FCH4))
EP_MWR$FCH4[EP_MWR$FCH4 <= (mean_CH4-sd_CH4_threshold)] <- NA
EP_MWR$FCH4[EP_MWR$FCH4 >= (mean_CH4 + sd_CH4_threshold)] <- NA

#Low turbulence Filter for CO2 data
EP_MWR$FC[EP_MWR$USTAR<=0.15]=NA         
EP_MWR$FC[EP_MWR$FC == -9999]<-NA
EP_MWR$FC[EP_MWR$FC_SS_TEST>=7] <- NA

#Standard deviation filtering for CO2
sd_CO2_threshold <- sd(na.omit(EP_MWR$FC))*3
mean_CO2 <- mean(na.omit(EP_MWR$FC))
EP_MWR$FC[EP_MWR$FC <= (mean_CO2-sd_CO2_threshold)] <- NA
EP_MWR$FC[EP_MWR$FC >= (mean_CO2 + sd_CO2_threshold)] <- NA


#Adjusts and filters other data parameters
EP_MWR$FH2O[EP_MWR$FH2O==-9999]<-NA
EP_MWR$FH2O[EP_MWR$FH2O_SS_TEST>=7] <- NA
EP_MWR$TA_EP <- EP_MWR$TA_EP -273.2

##Write this out to a new csv for future statistical analysis
########### 2024 DATA #############
setwd("C:/MontezumaData/2024_Data/EC_Data")
write.csv(EP_MWR, file = "MZA_2024_FinalOutput.csv", row.names = FALSE, quote=FALSE)

########## 2025 DATA #############
# setwd("C:/MontezumaData/2025_Data/EC_Data")
# write.csv(EP_MWR, file = "MZA_2025_FinalOutput.csv", row.names = FALSE, quote=FALSE)


##A few plots for consideration to help understand MWR data
setwd("C:/MontezumaData/2024_Data/EC_Data")
MWR_new <- read.csv("MZA_2024_FinalOutput.csv")

MWR_new$TIMESTAMP_START <- as.POSIXct(as.character(paste(MWR_new$TIMESTAMP_START)), format= "%Y%m%d%H%M")
MWR_new$TIMESTAMP_END <- as.POSIXct(as.character(paste(MWR_new$TIMESTAMP_END)), format= "%Y%m%d%H%M")

#General Time series of data
breaks_10days <- seq(
  from = min(MWR_new$TIMESTAMP_END),
  to   = max(MWR_new$TIMESTAMP_END),
  by   = "10 days"
)

p1 <- ggplot(MWR_new, aes(x = TIMESTAMP_END, y = FC)) +
  geom_line(size = 1, color = "darkgreen") +
  scale_x_datetime(
    breaks = breaks_10days,
    date_labels = "%b %d"
  ) +
  scale_y_continuous(limits = c(-50, 30)) +
  xlab("")+
  ylab(bquote(paste(CO[2], " Flux (umol ",m^-2, s^-1,")")))+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  theme(
    axis.title.y = element_text(size = 13),
    axis.text.x = element_blank(),   # hide x-axis text
    axis.ticks.x = element_blank(),  # hide x-axis ticks
    axis.title.x = element_blank(),   # hide x-axis title
    axis.text.y = element_text(size = 11), 
    legend.position = "none"
  )


p2 <- ggplot(data = MWR_new, aes(x=TIMESTAMP_END, y=FCH4))+
  geom_line(size=1, color ="steelblue")+
  scale_x_datetime(
    breaks = breaks_10days,
    date_labels = "%b %d"
  ) +
  xlab("Date")+
  ylab(bquote(paste(CH[4], " Flux (nmol ",m^-2, s^-1,")")))+
  scale_y_continuous(limits = c(-10,806))+
  theme_classic()+
  #labs(title = "Methane Flux (April - August 2025)",subtitle="Montezuma National Wildlife Refuge, Hidden Marsh")+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  theme(
    title = element_text(size = 13),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    axis.text.x = element_text(size = 11),  
    axis.text.y = element_text(size = 11), 
    legend.position = "none"
  )

library(patchwork)

setwd("C:/MontezumaData/2024_Data/EC_Data/FW_FluxMaps/Images/R_plots")
combined_plot <- p1 / p2 +
  plot_annotation(tag_levels = 'a')  # Optional: adds panel labels (a, b)

# Export the figure
ggsave(
  filename = "MWR_flux_timeseries_fullwidth.png",
  plot = combined_plot,
  width = 18,      # full page width in centimeters (≈7.1 inches)
  height = 12,     # adjust depending on how tall you want it
  units = "cm",
  dpi = 300        # high resolution for publication
)

# For vector output (preferred for journals):
ggsave(
  filename = "MWR_flux_timeseries_fullwidth.pdf",
  plot = combined_plot,
  width = 18,
  height = 12,
  units = "cm",
  useDingbats = FALSE
)

## DIURNAL PATTERN for flux in Montezuma
MWR_new$Time <- hour(MWR_new$TIMESTAMP_END) + minute(MWR_new$TIMESTAMP_END) / 60

#For FCH4
CH4_Means <- aggregate(FCH4 ~ Time, data = MWR_new, FUN = "mean")
CH4_SD <- aggregate(FCH4 ~ Time, data = MWR_new, FUN = "sd")

eb <- aes(ymax = FCH4 + CH4_SD$FCH4, ymin = FCH4 - CH4_SD$FCH4)
ggplot(data = CH4_Means, aes(x = Time, y = FCH4)) + 
  geom_line(size = 2, color="1") + 
  geom_ribbon(eb, alpha = 0.5, fill="4") +
  scale_x_continuous("Time of Day", expand=c(0,0), limits = c(0,24), breaks = c(0, 6, 12, 18, 23.5), 
                     labels = c("00:00", "06:00", "12:00", "18:00", "23:30")) +
  ylab(bquote(paste(CH[4], " Flux nmol/",m^2)))+
  scale_y_continuous(limits = c(0,600))+
  theme_classic() +
  labs(title="Diurnal Trends in Methane Flux (April - August 2025)",subtitle="Montezuma National Wildlife Refuge, Hidden Marsh")+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  theme(
    title = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 13),  
    axis.text.y = element_text(size = 13), 
    legend.position = "none"
  )


#For FCO2
CO2_Means <- aggregate(FC ~ Time, data = MWR_new, FUN = "mean")
CO2_SD <- aggregate(FC ~ Time, data = MWR_new, FUN = "sd")

eb2 <- aes(ymax = FC + CO2_SD$FC, ymin = FC - CO2_SD$FC)
ggplot(data = CO2_Means, aes(x = Time, y = FC)) + 
  geom_line(size = 2, color="1") + 
  geom_ribbon(eb2, alpha = 0.5, fill="3") +
  scale_x_continuous("Time of Day", expand=c(0,0), limits = c(0,24), breaks = c(0, 6, 12, 18, 23.5), 
                     labels = c("00:00", "06:00", "12:00", "18:00", "23:30")) +
  ylab(bquote(paste(CH[4], " Flux umol/",m^2)))+
  scale_y_continuous(limits = c(-30, 20))+
  theme_classic() +
  labs(title="Diurnal Trends in Carbon Dioxide Flux (April - August 2025)",subtitle="Montezuma National Wildlife Refuge, Hidden Marsh")+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  theme(
    title = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 13),  
    axis.text.y = element_text(size = 13), 
    legend.position = "none"
  )


plot(MWR_new$Datetime, MWR_new$FC)

##Potential causal parameters

plot(MWR_new$Wind_Speed, MWR_new$FCH4)
CH4_Wind <- lm(MWR_new$FCH4 ~ MWR_new$Wind_Speed)
abline(coefficients(CH4_Wind)[1], coefficients(CH4_Wind)[2], col="steelblue")



