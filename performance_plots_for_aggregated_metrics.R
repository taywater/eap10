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
    
    i <- 1
    
    #filter by observation well
    long_term_smp_metrics_select <- long_term_smp_metrics %>% 
      dplyr::filter(ow_uid == unique(long_term_smp_metrics$ow_uid)[i]) 
    
    #1.1.1 by event depth range ----
    #plot RPSU by event depth range for smp
    ggplot(long_term_smp_metrics_select, aes(x = eventdepth_range_in, group = eventdepth_range_in, y = rel_percentstorage)) + 
      geom_boxplot() +
      xlab("Event Depth Range (in)") +
      ylab("Relative Percent of Storage Used") + 
      ggtitle(paste(long_term_smp_metrics_select$smp_id[1], long_term_smp_metrics_select$ow_suffix[1])) +
      stat_summary(fun.data = give.n, geom= "text", position = position_dodge(width = 0.75))
    
    
    #1.1.2 by relative event depth range ----
    long_term_smp_metrics_select_rel <- long_term_smp_metrics_select %>% 
      dplyr::filter(relative_eventdepth_lookup_uid != 6) 
    
    long_term_smp_metrics_select_rel$relative_eventdepth_range_in = with(long_term_smp_metrics_select_rel, reorder(relative_eventdepth_range_in, relative_eventdepth_lookup_uid))
    
    ggplot(long_term_smp_metrics_select_rel, aes(x = relative_eventdepth_range_in, group = relative_eventdepth_lookup_uid, y = rel_percentstorage)) + 
      geom_boxplot() +
      xlab("Relative Event Depth Range (in)") +
      ylab("Relative Percent of Storage Used") + 
      ggtitle(paste(long_term_smp_metrics_select_rel$smp_id[1], long_term_smp_metrics_select_rel$ow_suffix[1])) +
      stat_summary(fun.data = give.n, geom= "text", position = position_dodge(width = 0.75))
    
  }
  
  #1.2 asset/functional type plots -----
  #1.2.1 subsurface -----
  
  subsurface_metrics <- dbGetQuery(mars, "select s.ow_uid, s.smp_id, s.ow_suffix, s.asset_type, s.lined, s.surface, s.infiltration_rate_inhr, s.rel_percentstorage, s.draindown_hr, s.dd_assessment_lookup_uid, s.overtopping, s.error_lookup_uid, s.rainfall_radarcell_event_uid, s.eventdepth_in, s.eventpeakintensity_inhr, s.eventdepth_lookup_uid, s.designdepth_in, s.relative_eventdepth_lookup_uid, e.eventdepth_range_in, e.eventdepth_description, r.relative_eventdepth_range_in, r.relative_eventdepth_description from performance.summary_ow_event_radarcell s
                                              left join performance.eventdepth_bin_lookup e on e.eventdepth_lookup_uid = s.eventdepth_lookup_uid
                                              left join performance.relative_eventdepth_bin_lookup r on r.relative_eventdepth_lookup_uid = s.relative_eventdepth_lookup_uid
                                              where s.surface = false")
  
  
  for(i in 1:length(unique(subsurface_metrics$asset_type))){
    
    i <- 1
    
    #filter by asset type
    subsurface_metrics_select <- subsurface_metrics %>% 
      dplyr::filter(asset_type == unique(subsurface_metrics$asset_type)[i])
    
    #1.2.1.1 by event depth range ----
    #plot RPSU by event depth range for smp
    ggplot(subsurface_metrics_select, aes(x = eventdepth_range_in, group = eventdepth_range_in, y = rel_percentstorage)) + 
      geom_boxplot() +
      xlab("Event Depth Range (in)") +
      ylab("Relative Percent of Storage Used") + 
      ggtitle(paste(subsurface_metrics_select$asset_type[1])) +
      stat_summary(fun.data = give.n, geom= "text", position = position_dodge(width = 0.75))
    
    
    #1.2.1.2 by relative event depth range ----
    subsurface_metrics_select_rel <- subsurface_metrics_select %>% 
      dplyr::filter(relative_eventdepth_lookup_uid != 6) 
    
    subsurface_metrics_select_rel$relative_eventdepth_range_in = with(subsurface_metrics_select_rel, reorder(relative_eventdepth_range_in, relative_eventdepth_lookup_uid))
    
    ggplot(subsurface_metrics_select_rel, aes(x = relative_eventdepth_range_in, group = relative_eventdepth_lookup_uid, y = rel_percentstorage)) + 
      geom_boxplot() +
      xlab("Relative Event Depth Range (in)") +
      ylab("Relative Percent of Storage Used") + 
      ggtitle(paste(subsurface_metrics_select_rel$asset_type[1])) +
      stat_summary(fun.data = give.n, geom= "text", position = position_dodge(width = 0.75))
    
  }
  
  #1.2.2 surface -------
  surface_metrics <- dbGetQuery(mars, "select s.ow_uid, s.smp_id, s.ow_suffix, s.asset_type, s.lined, s.surface, s.infiltration_rate_inhr, s.rel_percentstorage, s.draindown_hr, s.dd_assessment_lookup_uid, s.overtopping, s.error_lookup_uid, s.rainfall_radarcell_event_uid, s.eventdepth_in, s.eventpeakintensity_inhr, s.eventdepth_lookup_uid, s.designdepth_in, s.relative_eventdepth_lookup_uid, e.eventdepth_range_in, e.eventdepth_description, r.relative_eventdepth_range_in, r.relative_eventdepth_description from performance.summary_ow_event_radarcell s
                                              left join performance.eventdepth_bin_lookup e on e.eventdepth_lookup_uid = s.eventdepth_lookup_uid
                                              left join performance.relative_eventdepth_bin_lookup r on r.relative_eventdepth_lookup_uid = s.relative_eventdepth_lookup_uid
                                              where s.surface = true")
  
  
  for(i in 1:length(unique(surface_metrics$asset_type))){
    
    i <- 1
    
    #filter by asset type
    surface_metrics_select <- surface_metrics %>% 
      dplyr::filter(asset_type == unique(surface_metrics$asset_type)[i])
    
    #1.2.2.1 by event depth range ----
    #plot RPSU by event depth range for smp
    ggplot(surface_metrics_select, aes(x = eventdepth_range_in, group = eventdepth_range_in, y = draindown_hr)) + 
      geom_boxplot() +
      xlab("Event Depth Range (in)") +
      ylab("Draindown (hr)") + 
      ggtitle(paste(surface_metrics_select$asset_type[1])) +
      stat_summary(fun.data = give.n, geom= "text", position = position_dodge(width = 0.75))
    
    
    #1.2.2.2 by relative event depth range ----
    surface_metrics_select_rel <- surface_metrics_select %>% 
      dplyr::filter(relative_eventdepth_lookup_uid != 6) 
    
    surface_metrics_select_rel$relative_eventdepth_range_in = with(surface_metrics_select_rel, reorder(relative_eventdepth_range_in, relative_eventdepth_lookup_uid))
    
    ggplot(surface_metrics_select_rel, aes(x = relative_eventdepth_range_in, group = relative_eventdepth_lookup_uid, y = draindown_hr)) + 
      geom_boxplot() +
      xlab("Relative Event Depth Range (in)") +
      ylab("Draindown (hr)") + 
      ggtitle(paste(surface_metrics_select_rel$asset_type[1])) +
      stat_summary(fun.data = give.n, geom= "text", position = position_dodge(width = 0.75))
    
  }
  
  #1.3 by liner category
  #1.3.1 subsurface
