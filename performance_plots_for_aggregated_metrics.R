#EAP10 
#Performance plots for aggregated metrics
#Looking for: aggregated and binned performance metrics; boxplots showing performance ranges for various storm size bins for select long-term SMPs, SMPs aggregated by functional type, SMPs aggregated by liner category; 
#overtopping statistics presented as CDFs
#porous pavement performance relative to rejuvenating maintenance events (do we already have this?)
#other plots as time permits and insights present themselves


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

# 1.0 queries and plots

  # 1.1 long term smp plots (subsurface only)----
  
  long_term_smp_metrics <- dbGetQuery(mars, "select s.ow_uid, s.smp_id, s.ow_suffix, s.asset_type, s.lined, s.surface, s.infiltration_rate_inhr, s.rel_percentstorage, s.draindown_hr, s.dd_assessment_lookup_uid, s.overtopping, s.error_lookup_uid, s.rainfall_radarcell_event_uid, s.eventdepth_in, s.eventpeakintensity_inhr, s.eventdepth_lookup_uid, s.designdepth_in, s.relative_eventdepth_lookup_uid, e.eventdepth_range_in, e.eventdepth_description, r.relative_eventdepth_range_in, r.relative_eventdepth_description from performance.summary_ow_event_radarcell s
                                              left join performance.eventdepth_bin_lookup e on e.eventdepth_lookup_uid = s.eventdepth_lookup_uid
                                              left join performance.relative_eventdepth_bin_lookup r on r.relative_eventdepth_lookup_uid = s.relative_eventdepth_lookup_uid
                                              where smp_id in (select distinct(smp_id)
                                             from fieldwork.deployment_full where term = 'Long') and s.surface = false")
  give.n <- function(x){
    return(c(y = mean(x), label = length(x)))
  }
  
  
  for(i in 1:length(unique(long_term_smp_metrics$ow_uid))){
    
    #filter by observation well
    long_term_smp_metrics_select <- long_term_smp_metrics %>% 
      dplyr::filter(ow_uid == unique(long_term_smp_metrics$ow_uid)[i]) 
    
    deployment_time <- dbGetQuery(mars, paste0("select extract(epoch from real_deployment_days)/86400 from fieldwork.deployment_records_check where ow_uid = ", long_term_smp_metrics$ow_uid[1])) %>% pull
    
    smp_id <- long_term_smp_metrics_select$smp_id[1]
    
    ow_suffix <- long_term_smp_metrics_select$ow_suffix[1]
    
    design_depth <- round(long_term_smp_metrics_select$designdepth_in[1], 2)
    
    type <- "smp/"
    
    #1.1.1 by event depth range ----
    #plot RPSU by event depth range for smp
    smp_rpsu_event_depth_plot <- ggplot(long_term_smp_metrics_select, 
                                         aes(x = eventdepth_range_in, group = eventdepth_range_in, y = rel_percentstorage)) + 
        geom_boxplot() +
        xlab("Event Depth Range (in)") +
        ylab("Relative Percent of Storage Used") + 
      ggtitle(paste(smp_id, ow_suffix, "|", deployment_time, "Days", "| Design Depth: ~", design_depth, "in"))  +
        stat_summary(fun.data = give.n, geom= "label", size = 1/4*text_size, label.size = NA) +
        theme(text = element_text(size = text_size))
    
    ggsave(filename = paste0(folder,date, type, smp_id, "_", ow_suffix, "_event_depth_RPSU.jpg"), plot = smp_rpsu_event_depth_plot, width = 10, height = 8, units = "in")
    
    #plot infitration rate by event depth range for smp
    smp_infiltration_event_depth_plot <- ggplot(long_term_smp_metrics_select, 
                                        aes(x = eventdepth_range_in, group = eventdepth_range_in, y = infiltration_rate_inhr)) + 
      geom_boxplot() +
      xlab("Event Depth Range (in)") +
      ylab("Infiltration Rate (in/hr)") + 
      ggtitle(paste(smp_id, ow_suffix, "|", deployment_time, paste0("Days (", round(deployment_time/30, 1)), "Months) Monitored")) +
      stat_summary(fun.data = give.n, geom= "label", size = 1/4*text_size, label.size = NA) +
      theme(text = element_text(size = text_size))
    
    ggsave(filename = paste0(folder,date, type, smp_id, "_", ow_suffix, "_event_depth_infiltration.jpg"), plot = smp_infiltration_event_depth_plot, width = 10, height = 8, units = "in")
    
    #plot draindown time by event depth range for smp
    smp_draindown_event_depth_plot <- ggplot(long_term_smp_metrics_select, 
                                        aes(x = eventdepth_range_in, group = eventdepth_range_in, y = draindown_hr)) + 
      geom_boxplot() +
      xlab("Event Depth Range (in)") +
      ylab("Draindown (hr)") + 
      ggtitle(paste(smp_id, ow_suffix, "|", deployment_time, paste0("Days (", round(deployment_time/30, 1)), "Months) Monitored")) +
      stat_summary(fun.data = give.n, geom= "label", size = 1/4*text_size, label.size = NA) +
      theme(text = element_text(size = text_size))
    
    ggsave(filename = paste0(folder,date, type, smp_id, "_", ow_suffix, "_event_depth_draindown.jpg"), plot = smp_draindown_event_depth_plot, width = 10, height = 8, units = "in")
    
    #1.1.2 by relative event depth range ----
    long_term_smp_metrics_select_rel <- long_term_smp_metrics_select %>% 
      dplyr::filter(relative_eventdepth_lookup_uid != 6) 
    
    long_term_smp_metrics_select_rel$relative_eventdepth_range_in = with(long_term_smp_metrics_select_rel, reorder(relative_eventdepth_range_in, relative_eventdepth_lookup_uid))
    
    #rpsu
    smp_rpsu_rel_plot <- ggplot(long_term_smp_metrics_select_rel, 
                                        aes(x = relative_eventdepth_range_in, 
                                            group = relative_eventdepth_lookup_uid, y = rel_percentstorage)) + 
      geom_boxplot() +
      xlab("Relative Event Depth Range (in) - Event Depth less Design Depth") +
      ylab("Relative Percent of Storage Used") + 
      ggtitle(paste(smp_id, ow_suffix)) +
      stat_summary(fun.data = give.n, geom= "label", size = 1/4*text_size, label.size = NA) +
      theme(text = element_text(size = text_size))
    
    ggsave(filename = paste0(folder,date, type, smp_id, "_", ow_suffix, "_relative_event_depth_RPSU.jpg"), plot = smp_rpsu_rel_plot, width = 10, height = 8, units = "in")
  
    #infiltration rate
    smp_infiltration_rel_plot <- ggplot(long_term_smp_metrics_select_rel, 
                              aes(x = relative_eventdepth_range_in, 
                                  group = relative_eventdepth_lookup_uid, y = infiltration_rate_inhr)) + 
      geom_boxplot() +
      xlab("Relative Event Depth Range (in) - Event Depth less Design Depth") +
      ylab("Infiltration Rate (in/hr)") + 
      ggtitle(paste(smp_id, ow_suffix)) +
      stat_summary(fun.data = give.n, geom= "label", size = 1/4*text_size, label.size = NA) +
      theme(text = element_text(size = text_size))
  
    ggsave(filename = paste0(folder,date, type, smp_id, "_", ow_suffix, "_relative_event_depth_infiltration.jpg"), plot = smp_infiltration_rel_plot, width = 10, height = 8, units = "in")
    
    #draindown 
    smp_draindown_rel_plot <- ggplot(long_term_smp_metrics_select_rel, 
                                        aes(x = relative_eventdepth_range_in, 
                                            group = relative_eventdepth_lookup_uid, y = draindown_hr)) + 
      geom_boxplot() +
      xlab("Relative Event Depth Range (in) - Event Depth less Design Depth") +
      ylab("Draindown (hr)") + 
      ggtitle(paste(smp_id, ow_suffix)) +
      stat_summary(fun.data = give.n, geom= "label", size = 1/4*text_size, label.size = NA) +
      theme(text = element_text(size = text_size))
    
    ggsave(filename = paste0(folder,date, type, smp_id, "_", ow_suffix, "_relative_event_depth_draindown.jpg"), plot = smp_draindown_rel_plot, width = 10, height = 8, units = "in")
    
  }
  
  
  #1.2 asset/functional type plots -----
  #1.2.1 subsurface -----
  
  subsurface_metrics <- dbGetQuery(mars, "select s.ow_uid, s.smp_id, s.ow_suffix, s.asset_type, s.lined, s.surface, s.infiltration_rate_inhr, s.rel_percentstorage, s.draindown_hr, s.dd_assessment_lookup_uid, s.overtopping, s.error_lookup_uid, s.rainfall_radarcell_event_uid, s.eventdepth_in, s.eventpeakintensity_inhr, s.eventdepth_lookup_uid, s.designdepth_in, s.relative_eventdepth_lookup_uid, e.eventdepth_range_in, e.eventdepth_description, r.relative_eventdepth_range_in, r.relative_eventdepth_description from performance.summary_ow_event_radarcell s
                                              left join performance.eventdepth_bin_lookup e on e.eventdepth_lookup_uid = s.eventdepth_lookup_uid
                                              left join performance.relative_eventdepth_bin_lookup r on r.relative_eventdepth_lookup_uid = s.relative_eventdepth_lookup_uid
                                              where s.surface = false")
  
  
  for(i in 1:length(unique(subsurface_metrics$asset_type))){
    
    #i <- 1
    
    #filter by asset type
    subsurface_metrics_select <- subsurface_metrics %>% 
      dplyr::filter(asset_type == unique(subsurface_metrics$asset_type)[i])
    
    asset_type <- subsurface_metrics_select$asset_type[1]
    
    type <- "Functional Type/"
    
    #1.2.1.1 by event depth range ----
    #plot RPSU by event depth range for smp
    asset_rpsu_event_depth_plot <- ggplot(subsurface_metrics_select, 
                                          aes(x = eventdepth_range_in, group = eventdepth_range_in, y = rel_percentstorage)) + 
      geom_boxplot() +
      xlab("Event Depth Range (in)") +
      ylab("Relative Percent of Storage Used") + 
      ggtitle(paste(subsurface_metrics_select$asset_type[1])) +
      stat_summary(fun.data = give.n, geom= "label", size = 1/4*text_size, label.size = NA) +
      theme(text = element_text(size = text_size))
    
    ggsave(filename = paste0(folder,date, type, asset_type, "_event_depth_rpsu.jpg"), plot = asset_rpsu_event_depth_plot, width = 10, height = 8, units = "in")
    
    #infiltration rate
    asset_infiltration_event_depth_plot <- ggplot(subsurface_metrics_select, 
                                          aes(x = eventdepth_range_in, group = eventdepth_range_in, y = infiltration_rate_inhr)) + 
      geom_boxplot() +
      xlab("Event Depth Range (in)") +
      ylab("Infiltration Rate (in/hr)") + 
      ggtitle(paste(subsurface_metrics_select$asset_type[1])) +
      stat_summary(fun.data = give.n, geom= "label", size = 1/4*text_size, label.size = NA) +
      theme(text = element_text(size = text_size))
    
    ggsave(filename = paste0(folder,date, type, asset_type, "_event_depth_infiltration.jpg"), plot = asset_infiltration_event_depth_plot, width = 10, height = 8, units = "in")
    
    #draindown hr
    asset_draindown_event_depth_plot <- ggplot(subsurface_metrics_select, 
                                                  aes(x = eventdepth_range_in, group = eventdepth_range_in, y = draindown_hr)) + 
      geom_boxplot() +
      xlab("Event Depth Range (in)") +
      ylab("Draindown (hr)") + 
      ggtitle(paste(subsurface_metrics_select$asset_type[1])) +
      stat_summary(fun.data = give.n, geom= "label", size = 1/4*text_size, label.size = NA) +
      theme(text = element_text(size = text_size))
    
    ggsave(filename = paste0(folder,date, type, asset_type, "_event_depth_draindown.jpg"), plot = asset_draindown_event_depth_plot, width = 10, height = 8, units = "in")
    
    #1.2.1.2 by relative event depth range ----
    subsurface_metrics_select_rel <- subsurface_metrics_select %>% 
      dplyr::filter(relative_eventdepth_lookup_uid != 6) 
    
    subsurface_metrics_select_rel$relative_eventdepth_range_in = with(subsurface_metrics_select_rel, reorder(relative_eventdepth_range_in, relative_eventdepth_lookup_uid))
    
    #rpsu
    asset_rpsu_rel_plot <- ggplot(subsurface_metrics_select_rel, 
                                          aes(x = relative_eventdepth_range_in, group = relative_eventdepth_lookup_uid, y = rel_percentstorage)) + 
      geom_boxplot() +
      xlab("Storm Size - Design Storm Size (in) - Event Depth less Design Depth") +
      ylab("Relative Percent of Storage Used") + 
      ggtitle(paste(subsurface_metrics_select_rel$asset_type[1])) +
      stat_summary(fun.data = give.n, geom= "label", size = 1/4*text_size, label.size = NA) +
      theme(text = element_text(size = text_size))
    
    ggsave(filename = paste0(folder,date, type, asset_type, "_relative_event_depth_rpsu.jpg"), plot = asset_rpsu_rel_plot, width = 10, height = 8, units = "in")
    
    #infiltration
    asset_infiltration_rel_plot <- ggplot(subsurface_metrics_select_rel, 
                                  aes(x = relative_eventdepth_range_in, group = relative_eventdepth_lookup_uid, y = infiltration_rate_inhr)) + 
      geom_boxplot() +
      xlab("Storm Size - Design Storm Size (in) - Event Depth less Design Depth") +
      ylab("Infiltration Rate (in/hr)") + 
      ggtitle(paste(subsurface_metrics_select_rel$asset_type[1])) +
      stat_summary(fun.data = give.n, geom= "label", size = 1/4*text_size, label.size = NA) +
      theme(text = element_text(size = text_size))
    
    ggsave(filename = paste0(folder,date, type, asset_type, "_relative_event_depth_infiltration.jpg"), plot = asset_infiltration_rel_plot, width = 10, height = 8, units = "in")
    
    #draindown
    asset_draindown_rel_plot <- ggplot(subsurface_metrics_select_rel, 
                                          aes(x = relative_eventdepth_range_in, group = relative_eventdepth_lookup_uid, y = draindown_hr)) + 
      geom_boxplot() +
      xlab("Storm Size - Design Storm Size (in) - Event Depth less Design Depth") +
      ylab("Draindown (hr)") + 
      ggtitle(paste(subsurface_metrics_select_rel$asset_type[1])) +
      stat_summary(fun.data = give.n, geom= "label", size = 1/4*text_size, label.size = NA) +
      theme(text = element_text(size = text_size))
    
    ggsave(filename = paste0(folder,date, type, asset_type, "_relative_event_depth_draindown.jpg"), plot = asset_draindown_rel_plot, width = 10, height = 8, units = "in")
    
  }
  
  #1.2.2 surface -------
  surface_metrics <- dbGetQuery(mars, "select s.ow_uid, s.smp_id, s.ow_suffix, s.asset_type, s.lined, s.surface, s.infiltration_rate_inhr, s.rel_percentstorage, s.draindown_hr, s.dd_assessment_lookup_uid, s.overtopping, s.error_lookup_uid, s.rainfall_radarcell_event_uid, s.eventdepth_in, s.eventpeakintensity_inhr, s.eventdepth_lookup_uid, s.designdepth_in, s.relative_eventdepth_lookup_uid, e.eventdepth_range_in, e.eventdepth_description, r.relative_eventdepth_range_in, r.relative_eventdepth_description from performance.summary_ow_event_radarcell s
                                              left join performance.eventdepth_bin_lookup e on e.eventdepth_lookup_uid = s.eventdepth_lookup_uid
                                              left join performance.relative_eventdepth_bin_lookup r on r.relative_eventdepth_lookup_uid = s.relative_eventdepth_lookup_uid
                                              where s.surface = true")
  
  
  for(i in 1:length(unique(surface_metrics$asset_type))){
    
    #i <- 1
    
    #filter by asset type
    surface_metrics_select <- surface_metrics %>% 
      dplyr::filter(asset_type == unique(surface_metrics$asset_type)[i])
    
    asset_type <- surface_metrics_select$asset_type[1]
    
    type <- "Functional Type/"
    
    #1.2.2.1 by event depth range ----
    #plot RPSU by event depth range for smp
    asset_draindown_event_depth_plot <- ggplot(surface_metrics_select, aes(x = eventdepth_range_in, group = eventdepth_range_in, y = draindown_hr)) + 
      geom_boxplot() +
      xlab("Storm Size Range (in)") +
      ylab("Draindown (hr)") + 
      ggtitle(paste(surface_metrics_select$asset_type[1])) +
      stat_summary(fun.data = give.n, geom= "label", size = 1/4*text_size, label.size = NA) +
      theme(text = element_text(size = text_size))
    
    ggsave(filename = paste0(folder,date, type, asset_type, "_event_depth_draindown.jpg"), plot = asset_draindown_event_depth_plot, width = 10, height = 8, units = "in")
    
    #1.2.2.2 by relative event depth range ----
    surface_metrics_select_rel <- surface_metrics_select %>% 
      dplyr::filter(relative_eventdepth_lookup_uid != 6) 
    
    surface_metrics_select_rel$relative_eventdepth_range_in = with(surface_metrics_select_rel, reorder(relative_eventdepth_range_in, relative_eventdepth_lookup_uid))
    
    asset_draindown_rel_plot <- ggplot(surface_metrics_select_rel, 
                                               aes(x = relative_eventdepth_range_in, 
                                                   group = relative_eventdepth_lookup_uid, y = draindown_hr)) + 
      geom_boxplot() +
      xlab("Storm Size - Design Storm Size (in)") +
      ylab("Draindown (hr)") + 
      ggtitle(paste(surface_metrics_select_rel$asset_type[1])) +
      stat_summary(fun.data = give.n, geom= "label", size = 1/4*text_size, label.size = NA) +
      theme(text = element_text(size = text_size))
    
    ggsave(filename = paste0(folder,date, type, asset_type, "_relative_event_depth_draindown.jpg"), plot = asset_draindown_rel_plot, width = 10, height = 8, units = "in")
    
  }
  
  #1.3 by liner category
  #1.3.1 subsurface
  subsurface_metrics <- subsurface_metrics
  
  for(i in 1:length(unique(subsurface_metrics$lined))){
    
    #i <-1
    
    #filter by lined type
    subsurface_metrics_select <- subsurface_metrics %>% 
      dplyr::filter(lined == unique(subsurface_metrics$lined)[i])
    
    lined <- subsurface_metrics_select$lined[1]
    
    lined_type <- if(lined == 0){
      "Unlined"
    }else if(lined == 1){
      "Lined"
    }
    
    type = "Liner Category/"
    
    #1.3.1.1 by event depth range ----
    #plot RPSU by event depth range for smp
    lined_rpsu_event_depth_plot <- ggplot(subsurface_metrics_select, 
                                          aes(x = eventdepth_range_in, group = eventdepth_range_in, y = rel_percentstorage)) + 
      geom_boxplot() +
      xlab("Event Depth Range (in)") +
      ylab("Relative Percent of Storage Used") + 
      ggtitle(paste("Subsurface", lined_type)) +
      stat_summary(fun.data = give.n, geom= "label", size = 1/4*text_size, label.size = NA) +
      theme(text = element_text(size = text_size))
    
    ggsave(filename = paste0(folder,date,type, "Subsurface_", lined_type, "_event_depth_rpsu.jpg"), plot = lined_rpsu_event_depth_plot, width = 10, height = 8, units = "in")
    
    #infiltration rate
    lined_infiltration_event_depth_plot <- ggplot(subsurface_metrics_select, 
                                                  aes(x = eventdepth_range_in, group = eventdepth_range_in, y = infiltration_rate_inhr)) + 
      geom_boxplot() +
      xlab("Event Depth Range (in)") +
      ylab("Infiltration Rate (in/hr)") + 
      ggtitle(paste("Subsurface", lined_type)) +
      stat_summary(fun.data = give.n, geom= "label", size = 1/4*text_size, label.size = NA) +
      theme(text = element_text(size = text_size))
    
    ggsave(filename = paste0(folder,date, type, "Subsurface_", lined_type, "_event_depth_infiltration.jpg"), plot = lined_infiltration_event_depth_plot, width = 10, height = 8, units = "in")
    
    #draindown hr
    lined_draindown_event_depth_plot <- ggplot(subsurface_metrics_select, 
                                               aes(x = eventdepth_range_in, group = eventdepth_range_in, y = draindown_hr)) + 
      geom_boxplot() +
      xlab("Event Depth Range (in)") +
      ylab("Draindown (hr)") + 
      ggtitle(paste("Subsurface", lined_type)) +
      stat_summary(fun.data = give.n, geom= "label", size = 1/4*text_size, label.size = NA) +
      theme(text = element_text(size = text_size))
    
    ggsave(filename = paste0(folder,date, type, "Subsurface_", lined_type, "_event_depth_draindown.jpg"), plot = lined_draindown_event_depth_plot, width = 10, height = 8, units = "in")
    
    #1.3.1.2 by relative event depth range ----
    subsurface_metrics_select_rel <- subsurface_metrics_select %>% 
      dplyr::filter(relative_eventdepth_lookup_uid != 6) 
    
    subsurface_metrics_select_rel$relative_eventdepth_range_in = with(subsurface_metrics_select_rel, reorder(relative_eventdepth_range_in, relative_eventdepth_lookup_uid))
    
    
    #rpsu
    lined_rpsu_rel_plot <- ggplot(subsurface_metrics_select_rel, 
                                  aes(x = relative_eventdepth_range_in, group = relative_eventdepth_lookup_uid, y = rel_percentstorage)) + 
      geom_boxplot() +
      xlab("Storm Size - Design Storm Size (in) - Event Depth less Design Depth") +
      ylab("Relative Percent of Storage Used") + 
      ggtitle(paste("Subsurface", lined_type)) +
      stat_summary(fun.data = give.n, geom= "label", size = 1/4*text_size, label.size = NA) +
      theme(text = element_text(size = text_size))
    
    ggsave(filename = paste0(folder,date, type, "Subsurface_", lined_type,"_relative_event_depth_rpsu.jpg"), plot = lined_rpsu_rel_plot, width = 10, height = 8, units = "in")
    
    #infiltration
    lined_infiltration_rel_plot <- ggplot(subsurface_metrics_select_rel, 
                                          aes(x = relative_eventdepth_range_in, group = relative_eventdepth_lookup_uid, y = infiltration_rate_inhr)) + 
      geom_boxplot() +
      xlab("Storm Size - Design Storm Size (in) - Event Depth less Design Depth") +
      ylab("Infiltration Rate (in/hr)") + 
      ggtitle(paste("Subsurface", lined_type)) +
      stat_summary(fun.data = give.n, geom= "label", size = 1/4*text_size, label.size = NA) +
      theme(text = element_text(size = text_size))
    
    ggsave(filename = paste0(folder,date, type, "Subsurface_", lined_type, "_relative_event_depth_infiltration.jpg"), plot = lined_infiltration_rel_plot, width = 10, height = 8, units = "in")
    
    #draindown
    lined_draindown_rel_plot <- ggplot(subsurface_metrics_select_rel, 
                                       aes(x = relative_eventdepth_range_in, group = relative_eventdepth_lookup_uid, y = draindown_hr)) + 
      geom_boxplot() +
      xlab("Storm Size - Design Storm Size (in) - Event Depth less Design Depth") +
      ylab("Draindown (hr)") + 
      ggtitle(paste("Subsurface", lined_type)) +
      stat_summary(fun.data = give.n, geom= "label", size = 1/4*text_size, label.size = NA) +
      theme(text = element_text(size = text_size))
    
    ggsave(filename = paste0(folder,date, type, "Subsurface_", lined_type, "_relative_event_depth_draindown.jpg"), plot = lined_draindown_rel_plot, width = 10, height = 8, units = "in")
    
    
  }
  
  #1.3.2 surface -----
  for(i in 1:length(unique(surface_metrics$lined))){
    
    #i <- 1
    
    #filter by asset type
    surface_metrics_select <- surface_metrics %>% 
      dplyr::filter(lined == unique(surface_metrics$lined)[i])
    
    lined <- surface_metrics_select$lined[1]
    
    lined_type <- if(lined == 0){
      "Unlined"
    }else if(lined == 1){
      "Lined"
    }
    
    type <- "Liner Category/"
    
    #1.2.2.1 by event depth range ----
    #plot RPSU by event depth range for smp
    lined_draindown_event_depth_plot <- ggplot(surface_metrics_select, aes(x = eventdepth_range_in, group = eventdepth_range_in, y = draindown_hr)) + 
      geom_boxplot() +
      xlab("Storm Size Range (in)") +
      ylab("Draindown (hr)") + 
      ggtitle(paste("Surface", lined_type)) +
      stat_summary(fun.data = give.n, geom= "label", size = 1/4*text_size, label.size = NA) +
      theme(text = element_text(size = text_size))
    
    ggsave(filename = paste0(folder,date, type, "Surface_", lined_type, "_event_depth_draindown.jpg"), plot = lined_draindown_event_depth_plot, width = 10, height = 8, units = "in")
    
    #1.2.2.2 by relative event depth range ----
    surface_metrics_select_rel <- surface_metrics_select %>% 
      dplyr::filter(relative_eventdepth_lookup_uid != 6) 
    
    surface_metrics_select_rel$relative_eventdepth_range_in = with(surface_metrics_select_rel, reorder(relative_eventdepth_range_in, relative_eventdepth_lookup_uid))
    
    lined_draindown_rel_plot <- ggplot(surface_metrics_select_rel, 
                                       aes(x = relative_eventdepth_range_in, 
                                           group = relative_eventdepth_lookup_uid, y = draindown_hr)) + 
      geom_boxplot() +
      xlab("Storm Size - Design Storm Size (in)") +
      ylab("Draindown (hr)") + 
      ggtitle(paste("Surface", lined_type)) +
      stat_summary(fun.data = give.n, geom= "label", size = 1/4*text_size, label.size = NA) +
      theme(text = element_text(size = text_size))
    
    ggsave(filename = paste0(folder,date, type, "Surface_", lined_type, "_relative_event_depth_draindown.jpg"), plot = lined_draindown_rel_plot, width = 10, height = 8, units = "in")
    
  }
  
  
