#comparing observed and simulated metrics for EAP10
#Nick Manna
#started 6/7/21


# 0.0 SETUP-----
#clear env and load libraries
rm(list=ls())
library(pwdgsi)
library(odbc)
library(lubridate)
library(tidyverse)
library(stats)
library(gridExtra)
library(grid)
library(gtable)
library(ggtext)

#connection 
mars <- odbc::dbConnect(odbc::odbc(), "mars_data")

#folder root to save plots 
folder <- "//pwdoows/oows/Watershed Sciences/GSI Monitoring/06 Special Projects/34 PWDGSI metrics calculations/EAP10/"
dir.create(folder, showWarnings = FALSE)

date <- "202111118_aggregatedplots"
dir.create(paste0(folder, date), showWarnings = FALSE)

#font size 
text_size = 22

# 1.1 long term smp plots (subsurface only)----

subsurface_metrics <- dbGetQuery(mars, "select s.ow_uid, q.eventdatastart_edt, q.eventdepth_in, o.smp_id, o.ow_suffix, s.percentstorage, s.observed_simulated_lookup_uid from metrics.percentstorage s
                                              left join data.radar_event q on s.radar_event_uid = q.radar_event_uid
                                              left join fieldwork.ow o on s.ow_uid = o.ow_uid 
                                              where s.relative = true")

subsurface_metrics <- filter(subsurface_metrics, observed_simulated_lookup_uid == 1)

subsurface_metrics <- mutate(subsurface_metrics, eventdepth_lookup_uid = 
    ifelse(eventdepth_in < 0.5, 1,
           ifelse(eventdepth_in < 1, 2, 
                  ifelse(eventdepth_in < 1.5, 3, 
                         ifelse(eventdepth_in < 2, 4,
                                ifelse(eventdepth_in < 3, 5, 6))))))

subsurface_metrics <- left_join(subsurface_metrics, bins)

bins <- dbGetQuery(mars, "select * from metrics.eventdepth_bin_lookup")

ows <- unique(subsurface_metrics$ow_uid)

for(i in 1:length(ows)){
  
  site_data <- filter(subsurface_metrics, ow_uid == ows[i])
  
  rpsu_plot <- ggplot(data = site_data, aes(x = eventdatastart_edt, y = percentstorage, color = eventdepth_range_in, shape = eventdepth_range_in)) + 
    geom_point() +
    xlab("Date") + 
    ylab("Relative Percent of Storage Used") +
    ggtitle(paste(site_data$smp_id[1], site_data$ow_suffix[1], "Full Period of Record"), paste("Monitored Events:", nrow(site_data)))+
    labs(color = "Event Depth (in)", shape = "Event Depth (in)") +
    theme(text = element_text(size = text_size))  
  
  ggsave(filename = paste(folder,date, paste0(site_data$smp_id[1], "_", site_data$ow_suffix[1], "_rpsu.png"), sep = "/"), plot = rpsu_plot, width = 10, height = 8, units = "in")
}

