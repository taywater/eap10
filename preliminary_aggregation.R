#Preliminary Aggregation
#Pulling from database tables, current as of 4/13/21
#Move toward site level aggregation and then type aggregation (ie subsurface unlined)
#get averages, interquartile range, etc 
#create scatterplots for locations 
#box plots for lined / unlined with subsurface or surface

#surface / subsurface toggling is manual

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
  
  #get the fields i want from the db
  summary <- dbGetQuery(mars, "select ps.ow_uid, 
                        ps.smp_id, 
                        ps.ow_suffix, 
                        ps.infiltration_rate_inhr, 
                        ps.rel_percentstorage, 
                        ps.draindown_hr,
                        ps.dd_assessment_lookup_uid,
                        ps.overtopping,
                        ps.rainfall_radarcell_event_uid,
                        ps.eventdepth_in,
                        ps.eventpeakintensity_inhr, 
                        gu.lined,
                        gu.surface
                        from performance_summary_ow_event_radarcell ps
                        left join greenit_unified gu on ps.ow_uid = gu.ow_uid
                        where gu.subsurface = TRUE")
  ###SURFACE TOGGLE ^

  #folder
  folder <- "//pwdoows/OOWS/Watershed Sciences/GSI Monitoring/03 Reports and Presentations/01 Regulatory/EAP_Y10/Y10_Analysis/Preliminary Aggregation/"
  
  
#1 BY LOCATION-----

  #1.1 CREATE SUMMARIES ----
  #summarize draindown
  location_draindown_summary <- summary %>% 
    group_by(smp_id, ow_suffix, ow_uid) %>% 
    #dplyr::filter(lined == 1) %>% 
    #dplyr::filter(eventdepth_in > 0.74) %>%
    summarize(
      event_count = n(), 
      draindown_count = sum(!is.na(draindown_hr)), 
      avg_draindown = mean(draindown_hr, na.rm = TRUE),
      dda_count_1 = sum(dd_assessment_lookup_uid == 1), 
      dda_count_2 = sum(dd_assessment_lookup_uid == 2), 
      dda_count_3 = sum(dd_assessment_lookup_uid == 3), 
      dda_count_4 = sum(dd_assessment_lookup_uid == 4), 
      dda_count_5 = sum(dd_assessment_lookup_uid == 5),
      min_draindown = min(draindown_hr, na.rm = TRUE), 
      x_quarter = quantile(draindown_hr, 0.25, na.rm = TRUE), 
      median = quantile(draindown_hr, 0.5, na.rm = TRUE), 
      x_three_qtr = quantile(draindown_hr, 0.75, na.rm = TRUE),
      max_draindown = max(draindown_hr, na.rm = TRUE)
      # x = quantile(draindown_hr, c(0.25, 0.5, 0.75), na.rm = TRUE),
      # q = c(0.25, 0.5, 0.75)
    )
  
  #summarize percent storage
  location_rpsu_summary <- summary %>% 
    group_by(smp_id, ow_suffix, ow_uid) %>% 
    #dplyr::filter(lined == 1) %>% 
    #dplyr::filter(eventdepth_in > 0.74) %>%
    summarize(
      event_count = n(), 
      rpsu_count = sum(!is.na(rel_percentstorage)), 
      avg_rpsu = mean(rel_percentstorage, na.rm = TRUE),
      min_rpsu = min(rel_percentstorage, na.rm = TRUE), 
      x_quarter = quantile(rel_percentstorage, 0.25, na.rm = TRUE), 
      median = quantile(rel_percentstorage, 0.5, na.rm = TRUE), 
      x_three_qtr = quantile(rel_percentstorage, 0.75, na.rm = TRUE),
      max_rpsu = max(rel_percentstorage, na.rm = TRUE)
    )
  
  #summarize infiltration
  location_infiltration_summary <- summary %>% 
    group_by(smp_id, ow_suffix, ow_uid) %>% 
    #dplyr::filter(lined == 1) %>% 
    #dplyr::filter(eventdepth_in > 0.74) %>%
    summarize(
      event_count = n(), 
      infiltration_count = sum(!is.na(infiltration_rate_inhr)), 
      avg_infiltration_inhr = mean(infiltration_rate_inhr, na.rm = TRUE),
      min_infiltration_inhr = min(infiltration_rate_inhr, na.rm = TRUE), 
      x_quarter = quantile(infiltration_rate_inhr, 0.25, na.rm = TRUE), 
      median = quantile(infiltration_rate_inhr, 0.5, na.rm = TRUE), 
      x_three_qtr = quantile(infiltration_rate_inhr, 0.75, na.rm = TRUE),
      max_infiltration_inhr = max(infiltration_rate_inhr, na.rm = TRUE)
    )
  
  #summarize overtopping
  location_overtopping_summary <- summary %>% 
    group_by(smp_id, ow_suffix, ow_uid, lined) %>% 
    #dplyr::filter(lined == 1) %>% 
    #dplyr::filter(eventdepth_in > 0.74) %>%
    summarize(
      event_count = n(), 
      overtopping_count = sum(!is.na(overtopping)), 
      overtopping_true = sum(overtopping == 1),
      overtopping_percentage = overtopping_true/overtopping_count*100
    )
  
  #1.2 CREATE PLOTS ---------
  
  subsurface_location_folder <- paste0(folder, "subsurface_location/")
  
  #Draindown vs Count
  dd_count_plot <- ggplot(data = location_draindown_summary, mapping = aes(x = event_count, y = draindown_count)) + 
    geom_point() +
    geom_abline(slope = 1) +
    xlab("Event Count") + 
    ylab("Draindown Count") +
    ggtitle("Event Count vs Draindown Count (with 1:1 line)")
  
  dd_count_plot
  
  ggsave(filename = paste0(subsurface_location_folder, "Event Count vs Draindown Count", ".png"), plot = dd_count_plot, width = 10, height = 8, units = "in")
  
  #Draindown Average vs Count
  dd_mean_vs_count_plot <- ggplot(data = location_draindown_summary, mapping = aes(x = draindown_count, y = avg_draindown)) +
    geom_point() +
    xlab("Draindown Count") + 
    ylab("Average Draindown (hr)") +
    ggtitle("Draindown Count vs Average Draindown (hr)")

  dd_mean_vs_count_plot
  
  ggsave(filename = paste0(subsurface_location_folder, "Draindown Count vs Average Draindown", ".png"), plot = dd_mean_vs_count_plot, width = 10, height = 8, units = "in")
  
  #Mean RPSU vs Count
  rpsu_mean_vs_count_plot <- ggplot(location_rpsu_summary, mapping = aes(x = event_count, y = avg_rpsu)) +
    geom_point() +
    xlab("Event Count") + 
    ylab("Average RPSU") + 
    ggtitle("Event Count vs Relative Percent Storage Used")
  
  rpsu_mean_vs_count_plot
  
  ggsave(filename = paste0(subsurface_location_folder, "Event Count vs Relative Percent Storage Used", ".png"), plot = rpsu_mean_vs_count_plot, width = 10, height = 8, units = "in")

  #Mean RPSU vs Median RPSU
  rpsu_mean_vs_median_plot <- ggplot(location_rpsu_summary, aes(x = avg_rpsu, y = median, colour = event_count)) + 
    geom_point() +
    geom_abline(slope = 1) +
    xlab("Average RPSU") + 
    ylab("Median RPSU") + 
    ggtitle("Average vs Median Relative Percent of Storage Used")

  rpsu_mean_vs_median_plot 
  
  ggsave(filename = paste0(subsurface_location_folder, "Average vs Median Relative Percent of Storage Used", ".png"), plot = rpsu_mean_vs_median_plot, width = 10, height = 8, units = "in")

  #Infiltration Count vs Event Count
  inf_count_plot <- ggplot(location_infiltration_summary, aes(x = event_count, y = infiltration_count)) + 
    geom_point() + 
    geom_abline(slope = 1) + 
    xlab("Event Count") + 
    ylab("Infiltration Count") + 
    ggtitle("Event Count vs Infiltration Rate Count")

  inf_count_plot
  
  ggsave(filename = paste0(subsurface_location_folder, "Event Count vs Infiltration Rate Count", ".png"), plot = inf_count_plot, width = 10, height = 8, units = "in")
  
  #infiltration mean vs median
  inf_mean_vs_median_plot <- ggplot(location_infiltration_summary, aes(x = avg_infiltration_inhr, y = median, colour = event_count)) + 
    geom_point() + 
    geom_abline(slope = 1) + 
    xlab("Average Infiltration Rate (inhr)") + 
    ylab("Median Infiltration Rate (inhr)") + 
    ggtitle("Average vs Median Infiltration Rates (in/hr)")

  inf_mean_vs_median_plot 
  
  ggsave(filename = paste0(subsurface_location_folder, "Average vs Median Infiltration Rates", ".png"), plot = inf_mean_vs_median_plot, width = 10, height = 8, units = "in")
  
  #overtopping percent vs event count
  overtopping_percent_plot <- ggplot(location_overtopping_summary, aes(x = event_count, y = overtopping_percentage)) + 
    geom_point() + 
    xlab("Event Count") + 
    ylab("Percent Overtopping") + 
    ggtitle("Event Count vs Percent Overtopping")

  overtopping_percent_plot
  
  ggsave(filename = paste0(subsurface_location_folder, "Event Count vs Percent Overtopping", ".png"), plot = overtopping_percent_plot, width = 10, height = 8, units = "in")
  
#2 by "SUPERTYPE" -----
  
  #2.1 CREATE SUMMARIES ----
  #summarize draindown
  subsurface_draindown_summary <- summary %>% 
    group_by(lined) %>% 
    #dplyr::filter(lined == 0) %>% 
    #dplyr::filter(eventdepth_in > 0.74) %>%
    summarize(
      event_count = n(), 
      draindown_count = sum(!is.na(draindown_hr)), 
      avg_draindown = mean(draindown_hr, na.rm = TRUE),
      dda_count_1 = sum(dd_assessment_lookup_uid == 1), 
      dda_count_2 = sum(dd_assessment_lookup_uid == 2), 
      dda_count_3 = sum(dd_assessment_lookup_uid == 3), 
      dda_count_4 = sum(dd_assessment_lookup_uid == 4), 
      dda_count_5 = sum(dd_assessment_lookup_uid == 5),
      min_draindown = min(draindown_hr, na.rm = TRUE), 
      x_quarter = quantile(draindown_hr, 0.25, na.rm = TRUE), 
      median = quantile(draindown_hr, 0.5, na.rm = TRUE), 
      x_three_qtr = quantile(draindown_hr, 0.75, na.rm = TRUE),
      max_draindown = max(draindown_hr, na.rm = TRUE)
      # x = quantile(draindown_hr, c(0.25, 0.5, 0.75), na.rm = TRUE),
      # q = c(0.25, 0.5, 0.75)
    )
  
  #summarize percent storage
  subsurface_rpsu_summary <- summary %>% 
    group_by(lined) %>% 
    #dplyr::filter(lined == 1) %>% 
    #dplyr::filter(eventdepth_in > 0.74) %>%
    summarize(
      event_count = n(), 
      rpsu_count = sum(!is.na(rel_percentstorage)), 
      avg_rpsu = mean(rel_percentstorage, na.rm = TRUE),
      min_rpsu = min(rel_percentstorage, na.rm = TRUE), 
      x_quarter = quantile(rel_percentstorage, 0.25, na.rm = TRUE), 
      median = quantile(rel_percentstorage, 0.5, na.rm = TRUE), 
      x_three_qtr = quantile(rel_percentstorage, 0.75, na.rm = TRUE),
      max_rpsu = max(rel_percentstorage, na.rm = TRUE)
    )
  
  #summarize infiltration
  subsurface_infiltration_summary <- summary %>% 
    group_by(lined) %>% 
    #dplyr::filter(lined == 1) %>% 
    #dplyr::filter(eventdepth_in > 0.74) %>%
    summarize(
      event_count = n(), 
      infiltration_count = sum(!is.na(infiltration_rate_inhr)), 
      avg_infiltration_inhr = mean(infiltration_rate_inhr, na.rm = TRUE),
      min_infiltration_inhr = min(infiltration_rate_inhr, na.rm = TRUE), 
      x_quarter = quantile(infiltration_rate_inhr, 0.25, na.rm = TRUE), 
      median = quantile(infiltration_rate_inhr, 0.5, na.rm = TRUE), 
      x_three_qtr = quantile(infiltration_rate_inhr, 0.75, na.rm = TRUE),
      max_infiltration_inhr = max(infiltration_rate_inhr, na.rm = TRUE)
    )
  
  #summarize overtopping
  subsurface_overtopping_summary <- summary %>% 
    group_by(lined) %>% 
    #dplyr::filter(lined == 1) %>% 
    #dplyr::filter(eventdepth_in > 0.74) %>%
    summarize(
      event_count = n(), 
      overtopping_count = sum(!is.na(overtopping)), 
      overtopping_true = sum(overtopping == 1),
      overtopping_percentage = overtopping_true/overtopping_count*100
    )
  
  #2.2 CREATE PLOTS ----
  
  subsurface_general_folder <- paste0(folder, "subsurface_general/")
  
  #draindown box plot
  ss_dd_box_plot <- ggplot(summary, aes(x = lined, y = draindown_hr)) + 
    geom_boxplot() + 
    ylab("Draindown (hr)") + 
    xlab("Lined") + 
    ggtitle("Subsurface Draindown (Lined vs Unlined)")

  ss_dd_box_plot  

  ggsave(filename = paste0(subsurface_general_folder, "Subsurface Draindown (Lined vs Unlined)", ".png"), plot = ss_dd_box_plot, width = 10, height = 8, units = "in")
  
  #infiltration rate box plot
  ss_inf_box_plot <- ggplot(summary, aes(x = lined, y = infiltration_rate_inhr)) + 
    geom_boxplot() + 
    ylab("Infiltration Rate (in/hr)") + 
    xlab("Lined") + 
    ggtitle("Subsurface Infiltration Rate (Lined vs Unlined)")
  
  ss_inf_box_plot  
  
  ggsave(filename = paste0(subsurface_general_folder, "Subsurface Infiltration Rate (Lined vs Unlined)", ".png"), plot = ss_inf_box_plot, width = 10, height = 8, units = "in")

  #percent storage box plot
  ss_rpsu_box_plot <- ggplot(summary, aes(x = lined, y = rel_percentstorage)) + 
    geom_boxplot() + 
    ylab("RPSU") + 
    xlab("Lined") + 
    ggtitle("Subsurface Relative Percent of Storage Used (Lined vs Unlined)")
  
  ss_rpsu_box_plot  
  
  ggsave(filename = paste0(subsurface_general_folder, "Subsurface Relative Percent of Storage Used (Lined vs Unlined)", ".png"), plot = ss_rpsu_box_plot, width = 10, height = 8, units = "in")

  #overtopping box plot
  ss_overtopping_box_plot <- ggplot(location_overtopping_summary, aes(x = lined, y = overtopping_percentage)) + 
    geom_boxplot() + 
    ylab("Overtopping Percentage") + 
    xlab("Lined") + 
    ggtitle("Overtopping Percentage (Lined vs Unlined)")   

  ss_overtopping_box_plot  
  
  ggsave(filename = paste0(subsurface_general_folder, "Subsurface Overtopping Percentage (Lined vs Unlined)", ".png"), plot = ss_overtopping_box_plot, width = 10, height = 8, units = "in")
  
  
  
    
  