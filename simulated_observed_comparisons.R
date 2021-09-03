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
mars <- odbc::dbConnect(odbc::odbc(), "mars_testing")

#folder root to save plots 
folder <- "O:/Watershed Sciences/GSI Monitoring/06 Special Projects/34 PWDGSI metrics calculations/EAP10/"

date <- "20210902/"

#font size 
text_size = 22

#folder organization
type <- "sim_obs_comp/"

# 1.1 long term smp plots (subsurface only)----

subsurface_metrics <- dbGetQuery(mars, "select s.ow_uid, s.smp_id, s.ow_suffix, s.asset_type, s.lined, s.surface, s.infiltration_rate_inhr, s.rel_percentstorage, s.draindown_hr, s.dd_assessment_lookup_uid, s.overtopping, s.error_lookup_uid, s.rainfall_radarcell_event_uid, s.eventdepth_in, s.eventpeakintensity_inhr, s.eventdepth_lookup_uid, s.designdepth_in, s.relative_eventdepth_lookup_uid, e.eventdepth_range_in, e.eventdepth_description, r.relative_eventdepth_range_in, r.relative_eventdepth_description, s.observed_simulated_lookup_uid from performance.summary_ow_event_radarcell s
                                              left join performance.eventdepth_bin_lookup e on e.eventdepth_lookup_uid = s.eventdepth_lookup_uid
                                              left join performance.relative_eventdepth_bin_lookup r on r.relative_eventdepth_lookup_uid = s.relative_eventdepth_lookup_uid
                                              where s.surface = false")


give.n <- function(x){
  return(c(y = mean(x), label = length(x)/2))
}


#compare overtopping - simulated events vs observed events
sub_sim <- subsurface_metrics %>% 
  dplyr::filter(observed_simulated_lookup_uid == 2)

sub_obs <- subsurface_metrics %>% 
  dplyr::filter(observed_simulated_lookup_uid == 1) %>% 
  dplyr::filter(ow_uid %in% sub_sim$ow_uid & rainfall_radarcell_event_uid %in% sub_sim$rainfall_radarcell_event_uid)

sub_all <- subsurface_metrics %>% 
  #dplyr::filter(observed_simulated_lookup_uid == 1) %>% 
  dplyr::filter(ow_uid %in% sub_sim$ow_uid & rainfall_radarcell_event_uid %in% sub_sim$rainfall_radarcell_event_uid) %>% 
  mutate("Observed" = case_when(observed_simulated_lookup_uid == 1 ~ "Observed", 
                                observed_simulated_lookup_uid == 2 ~ "Simulated"))

overtopping_summary <- sub_all %>% 
  dplyr::count(asset_type, Observed, overtopping)

write.csv(overtopping_summary, paste0(folder,date, "subsurface_overtopping_summary.csv"))
  
for(i in 1:length(unique(sub_all$asset_type))){
 
  #filter by asset type
  sub_all_select <- sub_all %>% 
    dplyr::filter(asset_type == unique(sub_all$asset_type)[i])
  
  counts <- sub_all_select %>% 
    dplyr::filter(observed_simulated_lookup_uid == 1) %>% 
    dplyr::group_by(eventdepth_range_in) %>% 
    dplyr::count()
  
  asset_type <- sub_all_select$asset_type[1]
  
  type <- "Functional Type/"
  
  #plot RPSU by event depth range for smp
  asset_rpsu_event_depth_plot <-  ggplot(sub_all_select, 
                                        aes(x = eventdepth_range_in, y = rel_percentstorage)) + 
    geom_boxplot(aes(fill = Observed)) +
    #geom_text(data = counts, aes(label = counts$n, x= counts$eventdepth_range_in))+
    xlab("Event Depth Range (in)") +
    ylab("Relative Percent of Storage Used") +
    ggtitle(paste(sub_all_select$asset_type[1])) +
    stat_summary(fun.data = give.n, geom= "label", size = 1/4*text_size, label.size = NA) +
    theme(text = element_text(size = text_size))
  
    ggsave(filename = paste0(folder,date, type, asset_type, "_event_depth_rpsu.jpg"), plot = asset_rpsu_event_depth_plot, width = 10, height = 8, units = "in")
     
}

#now let's do this for combined tree trench and trench  
  #filter by asset type
  sub_all_select <- sub_all %>% 
    dplyr::filter(asset_type == "Trench" | asset_type == "Tree Trench")
  
  counts <- sub_all_select %>% 
    dplyr::filter(observed_simulated_lookup_uid == 1) %>% 
    dplyr::group_by(eventdepth_range_in) %>% 
    dplyr::count()
  
  asset_type <- "Trench and Tree Trench"
  
  type <- "Functional Type/"
  
  #plot RPSU by event depth range for smp
  asset_rpsu_event_depth_plot <-  ggplot(sub_all_select, 
                                         aes(x = eventdepth_range_in, y = rel_percentstorage)) + 
    geom_boxplot(aes(fill = Observed)) +
    #geom_text(data = counts, aes(label = counts$n, x= counts$eventdepth_range_in))+
    xlab("Event Depth Range (in)") +
    ylab("Relative Percent of Storage Used") +
    ggtitle(paste("Trench and Tree Trench")) +
    stat_summary(fun.data = give.n, geom= "label", size = 1/4*text_size, label.size = NA) +
    theme(text = element_text(size = text_size))
  
  ggsave(filename = paste0(folder,date, type, asset_type, "_event_depth_rpsu.jpg"), plot = asset_rpsu_event_depth_plot, width = 10, height = 8, units = "in")
  

