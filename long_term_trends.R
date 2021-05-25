#Long Term Performance Trends
#Long term subsurface unlined SMPs - plot infiltration rates and draindown times over time and look for trends

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
mars <- odbc::dbConnect(odbc::odbc(), "mars_testing")

#folder root to save plots 
folder <- "O:/Watershed Sciences/GSI Monitoring/06 Special Projects/34 PWDGSI metrics calculations/EAP10/"

date <- "20210525/"

#font size 
text_size = 22

#folder organization
type <- "long_term_trends/"

# 1.0 queries and plots

# 1.1 long term smp plots (subsurface only)----

long_term_smp_metrics <- dbGetQuery(mars, "select s.ow_uid, s.smp_id, s.ow_suffix, s.asset_type, s.lined, s.surface, rre.eventdatastart_edt, s.infiltration_rate_inhr, s.rel_percentstorage, s.draindown_hr, s.dd_assessment_lookup_uid, s.overtopping, s.error_lookup_uid, s.rainfall_radarcell_event_uid, s.eventdepth_in, s.eventpeakintensity_inhr, s.eventdepth_lookup_uid, s.designdepth_in, s.relative_eventdepth_lookup_uid, e.eventdepth_range_in, e.eventdepth_description, r.relative_eventdepth_range_in, r.relative_eventdepth_description from performance.summary_ow_event_radarcell s
                                              left join performance.eventdepth_bin_lookup e on e.eventdepth_lookup_uid = s.eventdepth_lookup_uid
                                              left join performance.relative_eventdepth_bin_lookup r on r.relative_eventdepth_lookup_uid = s.relative_eventdepth_lookup_uid
                                              left join rainfall_radarcell_event rre on rre.rainfall_radarcell_event_uid = s.rainfall_radarcell_event_uid
                                              where smp_id in (select distinct(smp_id)
                                             from fieldwork.deployment_full where term = 'Long') and s.surface = false")

for(i in 1:length(unique(long_term_smp_metrics$ow_uid))){
  
 # i <- 1
  
  #filter by observation well
  long_term_smp_metrics_select <- long_term_smp_metrics %>% 
    dplyr::filter(ow_uid == unique(long_term_smp_metrics$ow_uid)[i]) 

  
  smp_id <- long_term_smp_metrics_select$smp_id[1]
  
  ow_suffix <- long_term_smp_metrics_select$ow_suffix[1]
  
  #plot infiltration rate vs time
  inf_plot <- ggplot(data = long_term_smp_metrics_select, aes(x = eventdatastart_edt, y = infiltration_rate_inhr, color = eventdepth_range_in, shape = eventdepth_range_in)) + 
    geom_point() +
    xlab("Date") + 
    ylab("Infiltration Rate (in/hr)") +
    #scale_colour_gradient(low = "blue", high = "red")+
    ggtitle(paste(smp_id, ow_suffix))+
    labs(color = "Event Depth (in)", shape = "Event Depth (in)") +
    theme(text = element_text(size = text_size))
  
  subtype <- "infiltration/"
  
  ggsave(filename = paste0(folder,date, type, subtype, smp_id, "_", ow_suffix, "_infiltration_rate.jpg"), plot = inf_plot, width = 10, height = 8, units = "in")
  
  draindown_plot <- ggplot(data = long_term_smp_metrics_select, aes(x = eventdatastart_edt, y = draindown_hr, color = eventdepth_range_in, shape = eventdepth_range_in)) + 
    geom_point() +
    xlab("Date") + 
    ylab("Draindown (hr)") +
    #scale_colour_gradient(low = "blue", high = "red")+
    ggtitle(paste(smp_id, ow_suffix))+
    labs(color = "Event Depth (in)", shape = "Event Depth (in)") +
    theme(text = element_text(size = text_size))
  
  subtype <- "draindown/"
  
  ggsave(filename = paste0(folder,date, type, subtype, smp_id, "_", ow_suffix, "_draindown.jpg"), plot = draindown_plot, width = 10, height = 8, units = "in")
  
  rpsu_plot <- ggplot(data = long_term_smp_metrics_select, aes(x = eventdatastart_edt, y = rel_percentstorage, color = eventdepth_range_in, shape = eventdepth_range_in)) + 
    geom_point() +
    xlab("Date") + 
    ylab("Relative Percent of Storage Used") +
   # scale_colour_gradient(low = "blue", high = "red")+
    ggtitle(paste(smp_id, ow_suffix))+
    labs(color = "Event Depth (in)", shape = "Event Depth (in)") +
    theme(text = element_text(size = text_size))
  
  subtype <- "percent_storage/"
  
  ggsave(filename = paste0(folder,date, type, subtype, smp_id, "_", ow_suffix, "_rpsu.jpg"), plot = rpsu_plot, width = 10, height = 8, units = "in")
  
}
