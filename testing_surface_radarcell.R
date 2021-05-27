# A LOOP TO TEST pwdgsi ON SURFACE systems
#set up for radarcell rain

rm(list=ls())
library(pwdgsi)
library(lubridate)
library(tidyverse)
library(stats)
library(gridExtra)
library(grid)
library(gtable)
library(ggtext)

#connection 
mars <- odbc::dbConnect(odbc::odbc(), "mars_testing")

#read in surface lined systems 
ow_testing_list <- read.csv("C:/Users/nicholas.manna/Documents/R/EAP10/csvs for pwdgsi testing/surface_lined_20210217.csv",
                            stringsAsFactors = FALSE) %>%  
  dplyr::rename("smp_id" = 1) %>% 
  dplyr::filter(!(smp_id %in% c('445-1-1', '211-1-1', '186-2-1'))) %>% 
  dplyr::filter(ow_suffix != 'OW1')

#create a stamp function, so date inputs fit the standard for pwdgsi functions #not sure how necessary this is
sf <- lubridate::stamp("2014-12-31")

#initialize an empty list, the length of ow_lesting_list
sum_list <- vector(mode = "list", length = length(ow_testing_list$start_date))

for(i in 1:length(ow_testing_list$start_date)){
#for(i in 1:10){
  #read dates and SMPs from testing list
  start_date <- sf(mdy(ow_testing_list$start_date[i]))#, sf(mdy(ow_testing_list$start_date[i+1])))
  end_date   <- sf(mdy(ow_testing_list$end_date[i]))#, sf(mdy(ow_testing_list$end_date[i+1])))
  target_id  <- ow_testing_list$smp_id[i] %>% as.character()#, ow_testing_list$smp_id[i+1] %>% as.character())
  ow_suffix  <- ow_testing_list$ow_suffix[i] %>% as.character()#, ow_testing_list$ow_suffix[i] %>% as.character())
  
  #create folders for plots and plots with errors
  folder <- (paste0("O:/Watershed Sciences/GSI Monitoring/06 Special Projects/34 PWDGSI metrics calculations/EAP10/20210413/surface_lined/", paste(target_id, ow_suffix, sep = "_")))
  error_folder = paste0(folder, "/error")
  dir.create(folder, showWarnings = FALSE)
  dir.create(error_folder, showWarnings = FALSE)
  
  #call snapshot
  snapshot <- marsFetchSMPSnapshot(con = mars, 
                                   smp_id = target_id, 
                                   ow_suffix = ow_suffix, 
                                   request_date = "today")
  
  #check if simulation should happen, based on available snapshot fields
  sim_true <- !(is.na(snapshot$storage_depth_ft)| is.na(snapshot$dcia_ft2) | is.na(snapshot$storage_volume_ft3) |
                  snapshot$storage_depth_ft == 0 | snapshot$dcia_ft2 == 0 | snapshot$storage_volume_ft3 == 0 |
                  (snapshot$infil_dsg_rate_inhr == 0 & is.na(snapshot$orifice_diam_in)) |
                  (is.na(snapshot$infil_dsg_rate_inhr) & is.na(snapshot$orifice_diam_in)) |
                  ((snapshot$infil_footprint_ft2 == 0 | is.na(snapshot$infil_footprint_ft)) & is.na(snapshot$orifice_diam_in)))
  
  #set sim_true to FALSE to skip simulations (comment out if you want to do simulations)
  sim_true <- FALSE
  
  #fetch monitoring data
  monitoringdata <- marsFetchMonitoringData(con = mars, 
                                            target_id = target_id, 
                                            ow_suffix = ow_suffix, 
                                            source = "radarcell",
                                            start_date = start_date, 
                                            end_date = end_date, 
                                            sump_correct = FALSE,
                                            debug = TRUE)
  
  rain_event_data <- monitoringdata[["Rain Event Data"]]
  rain_data <- monitoringdata[["Rainfall Data"]] %>% mutate(across(dtime_est), - hours(5))
  rain_data <- monitoringdata[["Rainfall Data"]]
  level_data <- monitoringdata[["Level Data"]]
  level_data <- monitoringdata[["Level Data"]]
  
  # check_61616_level <- level_data %>% dplyr::filter(rainfall_radarcell_event_uid == 61616)
  # check_61616_rain <- rain_data %>% dplyr::filter(rainfall_radarcell_event_uid == 61616)
  # 
  # check_join <- dplyr::full_join(check_61616_level, check_61616_rain, by = "dtime_est")
  # 
  # sum(check_join$rainfall_in, na.rm = TRUE)
  
  #join monitoring data in one table
  obs_data <- dplyr::full_join(monitoringdata[["Level Data"]], monitoringdata[["Rainfall Data"]], 
                               by = c("dtime_est", "radarcell_uid", "rainfall_radarcell_event_uid")) %>% 
    dplyr::arrange(dtime_est) %>%
    dplyr::mutate(across(c("level_ft", "ow_uid"), ~ zoo::na.locf(., na.rm = FALSE))) %>%
    dplyr::mutate(across(c("level_ft", "ow_uid"), ~ zoo::na.locf(., fromLast = TRUE))) %>% 
    dplyr::mutate(orifice_outflow_ft3 = marsUnderdrainOutflow_cf(dtime_est = dtime_est,
                                                                 waterlevel_ft = level_ft,
                                                                 orifice_height_ft = snapshot$assumption_orificeheight_ft,
                                                                 orifice_diam_in = snapshot$orifice_diam_in))
  
  #set initial water levels so the simulation starts at the same spot as observed
  initial_water_levels <- obs_data %>% 
    dplyr::group_by(rainfall_radarcell_event_uid) %>%
    dplyr::summarize(
      ft = dplyr::first(level_ft)
    )
  
  initial_water_levels <- initial_water_levels[complete.cases(initial_water_levels), ]
  
  print(i)
  print(target_id)
  
  #check_61616 <- dplyr::filter(obs_data, rainfall_radarcell_event_uid == 61616)
  
  #sum_61616 <- sum(check_61616$rainfall_in, na.rm = TRUE)
  
  
  #observed ---- 
  
  #create a summary table of observed data
  observed_summary <- obs_data %>%
    dplyr::arrange(dtime_est) %>% 
    dplyr::filter(is.na(rainfall_radarcell_event_uid) == FALSE) %>% #remove rows that had water level data but no event ID
    #dplyr::filter(rainfall_radarcell_event_uid == 159691) %>% 
    dplyr::group_by(rainfall_radarcell_event_uid) %>%
    dplyr::summarize(
      
      #Ow uid
      ow_uid = ow_uid[1],
      
      # first_point = dplyr::first(level_ft), 
      
      #Observed storage utilization
      percentstorageused_peak = marsPeakStorage_percent(waterlevel_ft = level_ft, storage_depth_ft = snapshot$storage_depth_ft) %>% round(4),
      
      #Observed relative storage utilization
      percentstorageused_relative = marsPeakStorage_percent(waterlevel_ft = level_ft - dplyr::first(level_ft), storage_depth_ft = snapshot$storage_depth_ft) %>% round(4),
      
      baseline = marsWaterLevelBaseline_ft(dtime_est = dtime_est, 
                                           level_ft = level_ft),
      
      infiltration_inhr = marsInfiltrationRate_inhr(event = rainfall_radarcell_event_uid,
                                                    dtime_est = dtime_est,
                                                    rainfall_in = rainfall_in,
                                                    snapshot$dcia_ft2,
                                                    snapshot$assumption_orificeheight_ft,
                                                    snapshot$orifice_diam_in,
                                                    storage_depth_ft = snapshot$storage_depth_ft,
                                                    #storage_depth_ft = 1,
                                                    storage_vol_ft3 = snapshot$storage_volume_ft3,
                                                    waterlevel_ft = level_ft,
                                                    depth_in = 6),
      
      #Draindown time
      draindown_hr = marsDraindown_hr(dtime_est = dtime_est,
                                      rainfall_in = rainfall_in,
                                      waterlevel_ft = level_ft),
      
      draindownAssessment = marsDraindownAssessment(level_ft = level_ft, 
                                                    eventdepth_in = rain_event_data$eventdepth_in[which(rain_event_data$rainfall_radarcell_event_uid == rainfall_radarcell_event_uid[1])], 
                                                    designdepth_in = snapshot$storage_volume_ft3/snapshot$dcia_ft2*12, 
                                                    storage_depth_ft = snapshot$storage_depth_ft, 
                                                    draindown_hr = draindown_hr, 
                                                    subsurface = TRUE, 
                                                    event_id_check = rainfall_radarcell_event_uid[1]), 
      
      overtop = marsOvertoppingCheck_bool(level_ft, snapshot$storage_depth_ft),
      
      peakReleaseRate_cfs = marsPeakReleaseRate_cfs(dtime_est, orifice_outflow_ft3 = orifice_outflow_ft3),
      
      orifice_volume_cf = round(sum(orifice_outflow_ft3),0),
      
      snapshot_uid = snapshot$snapshot_uid,
      
      observed_simulated_lookup_uid = 1
    )
  
  #other stuff ----
  
  
  
  #merge observed and simulated summaries if necessary
  if(sim_true){
    obs_sim_summary <- dplyr::bind_rows(observed_summary, sim_summary)
  }else{
    obs_sim_summary <- observed_summary
  }
  
  write.csv(observed_summary, paste0(folder, "/", target_id, "_", ow_suffix, "summary.csv"))
  ######
  #plots!
  # for(j in 1:length(rain_event_data$rainfall_radarcell_event_uid)){
  #   #j <-1
  #   #filter for each event
  #   selected_event <- obs_data %>%
  #     dplyr::filter(rainfall_radarcell_event_uid == rain_event_data$rainfall_radarcell_event_uid[j])
  #   rain_plot_data <- monitoringdata[["Rainfall Data"]] %>%
  #     dplyr::filter(rainfall_radarcell_event_uid == rain_event_data$rainfall_radarcell_event_uid[j])
  #   
  #   #if(rain_event_data$eventdepth_in[j] > 1.2){
  #     # skip plots with errors -900
  #     #if simulation ran, plot sim and obs. if not, just plot observed
  #     if(sim_true){#& obs_sim_summary$infiltration_inhr[j] != -900){
  #       sim_event <- simulated_data %>%
  #         dplyr::filter(rainfall_radarcell_event_uid == rain_event_data$rainfall_radarcell_event_uid[j])
  #       #plot observed and simulated data
  #       plot <- marsCombinedPlot(event = rain_event_data$rainfall_radarcell_event_uid[j],
  #                                structure_name = paste(target_id, ow_suffix),
  #                                obs_datetime = selected_event$dtime_est,
  #                                obs_level_ft = selected_event$level_ft,
  #                                sim_datetime = sim_event$dtime_est,
  #                                sim_level_ft = sim_event$Simulated_depth_ft,
  #                                storage_depth_ft = snapshot$storage_depth_ft,
  #                                orifice_show = TRUE,
  #                                orifice_height_ft = snapshot$assumption_orificeheight_ft,
  #                                rainfall_datetime = rain_plot_data$dtime_est,
  #                                rainfall_in = rain_plot_data$rainfall_in,
  #                                raingage = rain_plot_data$gage_uid,
  #                                infiltration_rate_inhr = obs_sim_summary$infiltration_inhr[j],
  #                                draindown_hr = paste(obs_sim_summary$draindown_hr[j], "| Score:", obs_sim_summary$draindownAssessment[j]),
  #                                percent_storage_relative = round(obs_sim_summary$percentstorageused_relative[j],2),
  #                                baseline_ft = obs_sim_summary$baseline[j])
  #     }else{#if(obs_sim_summary$infiltration_inhr[j] != -900){
  #       #   # plot observed data
  #       plot <- marsCombinedPlot(event = rain_event_data$rainfall_radarcell_event_uid[j],
  #                                structure_name = paste(target_id, ow_suffix),
  #                                obs_datetime = selected_event$dtime_est,
  #                                obs_level_ft = selected_event$level_ft,
  #                                storage_depth_ft = snapshot$storage_depth_ft,
  #                                orifice_show = TRUE,
  #                                orifice_height_ft = snapshot$assumption_orificeheight_ft,
  #                                rainfall_datetime = rain_plot_data$dtime_est,
  #                                rainfall_in = rain_plot_data$rainfall_in,
  #                                raingage = rain_plot_data$gage_uid,
  #                                infiltration_rate_inhr = obs_sim_summary$infiltration_inhr[j],
  #                                draindown_hr = paste(obs_sim_summary$draindown_hr[j], "| Score:", obs_sim_summary$draindownAssessment[j]),
  #                                percent_storage_relative = round(obs_sim_summary$percentstorageused_relative[j],2),
  #                                baseline_ft = obs_sim_summary$baseline[j])
  #       
  #     }
  #     #save plot
  #     #put errors in a different folder
  #     #if(obs_sim_summary$infiltration_inhr[j] > -900){
  #       ggplot2::ggsave(paste0(folder, "/", paste(target_id, ow_suffix, rain_event_data$rainfall_radarcell_event_uid[j], sep = "_"),".png"), plot = plot, width = 10, height = 8)
  #     #}else if(obs_sim_summary$infiltration_inhr[j] != -900){
  #       # ggplot2::ggsave(paste0(error_folder, "/", paste(target_id, ow_suffix, rain_event_data$rainfall_radarcell_event_uid[j], sep = "_"),".png"), plot = plot, width = 10, height = 8)
  #     #}
  #   }
  # }
  
  #writing to database
  #this has been excluded from the loops, for now
  # this can be commented if needed
  write_draindown <- marsWriteDraindownData(con = mars,
                                            draindown_hr = obs_sim_summary$draindown_hr,
                                            performance_draindown_assessment_lookup_uid = obs_sim_summary$draindownAssessment,
                                            ow_uid = obs_sim_summary$ow_uid,
                                            source = "radarcell",
                                            event_uid =obs_sim_summary$rainfall_radarcell_event_uid,
                                            snapshot_uid = obs_sim_summary$snapshot_uid,
                                            observed_simulated_lookup_uid =obs_sim_summary$observed_simulated_lookup_uid)

  write_infiltration <- marsWriteInfiltrationData(mars,
                                                  obs_sim_summary$infiltration_inhr,
                                                  obs_sim_summary$baseline,
                                                  obs_sim_summary$ow_uid,
                                                  source = "radarcell",
                                                  obs_sim_summary$rainfall_radarcell_event_uid,
                                                  obs_sim_summary$snapshot_uid,
                                                  obs_sim_summary$observed_simulated_lookup_uid)

  write_overtopping <- marsWriteOvertoppingData(mars,
                                                obs_sim_summary$overtop,
                                                obs_sim_summary$observed_simulated_lookup_uid,
                                                obs_sim_summary$ow_uid,
                                                source = "radarcell",
                                                obs_sim_summary$rainfall_radarcell_event_uid,
                                                obs_sim_summary$snapshot_uid)

  write_percent <- marsWritePercentStorageData(mars,
                                               obs_sim_summary$percentstorageused_peak,
                                               obs_sim_summary$percentstorageused_relative,
                                               obs_sim_summary$ow_uid,
                                               source = "radarcell",
                                               obs_sim_summary$rainfall_radarcell_event_uid,
                                               obs_sim_summary$snapshot_uid,
                                               obs_sim_summary$observed_simulated_lookup_uid)
  
  ######## 
}
