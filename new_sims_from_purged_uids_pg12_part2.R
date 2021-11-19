# A LOOP TO TEST pwdgsi ON SUBSURFACE systems
#set up for radarcell rain

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

#create a stamp function, so date inputs fit the standard for pwdgsi functions #not sure how necessary this is
sf <- lubridate::stamp("2014-12-31")

sim_candidate_query <- "select distinct o.ow_uid, o.smp_id, o.ow_suffix from admin.purge p left join fieldwork.ow o on o.ow_uid = p.ow_uid"

sim_candidates <- dbGetQuery(mars, sim_candidate_query)
sim_candidates$start_date <- NA
sim_candidates$end_date <- NA
sim_candidates$sim_possible <- FALSE

for(i in 1:nrow(sim_candidates)){
  start_date_query <- paste("select dtime_est from data.ow_leveldata_raw where ow_uid =", sim_candidates$ow_uid[i], "order by dtime_est asc limit 1")
  start_date <- dbGetQuery(mars, start_date_query) %>% pull(dtime_est) %>% sf
  if(length(start_date) > 0){sim_candidates$start_date[i] <- start_date}
  
  end_date_query <- paste("select dtime_est from data.ow_leveldata_raw where ow_uid =", sim_candidates$ow_uid[i], "order by dtime_est desc limit 1")  
  end_date <- dbGetQuery(mars, end_date_query) %>% pull(dtime_est) %>% sf
  if(length(end_date) > 0){sim_candidates$end_date[i] <- end_date}
  
  print(sim_candidates[i, ])
  
  target_id  <- sim_candidates$smp_id[i]
  ow_suffix  <- sim_candidates$ow_suffix[i]
  
  #call snapshot
  snapshot <- marsFetchSMPSnapshot(con = mars, 
                                   smp_id = target_id, 
                                   ow_suffix = ow_suffix, 
                                   request_date = "today")

  #check if simulation should happen, based on available snapshot fields
  #right now it will not happen so i comment out
  sim_candidates$sim_possible[i] <- !(is.na(snapshot$storage_depth_ft)| is.na(snapshot$dcia_ft2) | is.na(snapshot$storage_volume_ft3) |
                  snapshot$storage_depth_ft == 0 | snapshot$dcia_ft2 == 0 | snapshot$storage_volume_ft3 == 0 |
                  (snapshot$infil_dsg_rate_inhr == 0 & is.na(snapshot$orifice_diam_in)) |
                  (is.na(snapshot$infil_dsg_rate_inhr) & is.na(snapshot$orifice_diam_in)) |
                  ((snapshot$infil_footprint_ft2 == 0 | is.na(snapshot$infil_footprint_ft)) & is.na(snapshot$orifice_diam_in)))
}

sim_candidates <- filter(sim_candidates, sim_possible == TRUE, !is.na(start_date), start_date != end_date) %>% arrange(smp_id)

for(i in 67:nrow(sim_candidates)){
  bigfolder <- "//pwdoows/oows/Watershed Sciences/GSI Monitoring/06 Special Projects/34 PWDGSI metrics calculations/EAP10/20211115_post-purge/"
  folder <- (paste0(bigfolder, paste(sim_candidates$smp_id[i], sim_candidates$ow_suffix[i], sep = "_")))
  error_folder = paste0(folder, "/error")
  dir.create(bigfolder, showWarnings = FALSE)
  dir.create(folder, showWarnings = FALSE)
  dir.create(error_folder, showWarnings = FALSE)
  
  monitoringdata <-  marsFetchMonitoringData(con = mars, 
                                             target_id = sim_candidates$smp_id[i], 
                                             ow_suffix = sim_candidates$ow_suffix[i], 
                                             source = "radar",
                                             start_date = sim_candidates$start_date[i], 
                                             end_date = sim_candidates$end_date[i], 
                                             sump_correct = TRUE,
                                             debug = TRUE)
  
  snapshot <- marsFetchSMPSnapshot(con = mars, 
                                   smp_id = sim_candidates$smp_id[i], 
                                   ow_suffix = sim_candidates$ow_suffix[i], 
                                   request_date = "today")
  
  #join monitoring data in one table
  #to calculate summary statistics with summarize()
  obs_data <- dplyr::full_join(monitoringdata[["Level Data"]], monitoringdata[["Rainfall Data"]], 
                               by = c("dtime_est", "radar_uid", "radar_event_uid")) %>% 
    dplyr::arrange(dtime_est) %>%
    dplyr::mutate(across(c("level_ft", "ow_uid"), ~ zoo::na.locf(., na.rm = FALSE))) %>%
    dplyr::mutate(across(c("level_ft", "ow_uid"), ~ zoo::na.locf(., fromLast = TRUE))) %>% 
    dplyr::mutate(orifice_outflow_ft3 = marsUnderdrainOutflow_cf(dtime_est = dtime_est,
                                                                 waterlevel_ft = level_ft,
                                                                 orifice_height_ft = snapshot$assumption_orificeheight_ft,
                                                                 orifice_diam_in = snapshot$orifice_diam_in))
  
  
  #set initial water levels so the simulation starts at the same spot as observed
  initial_water_levels <- obs_data %>% 
    dplyr::group_by(radar_event_uid) %>%
    dplyr::summarize(ft = dplyr::first(level_ft)) %>% 
    filter(!is.na(radar_event_uid))
  
  print(paste("Simming", sim_candidates$smp_id[i]))
  
  simulated_data <- marsSimulatedLevelSeries_ft(dtime_est = monitoringdata[["Rainfall Data"]]$dtime_est,
                                                rainfall_in = monitoringdata[["Rainfall Data"]]$rainfall_in,
                                                event = monitoringdata[["Rainfall Data"]]$radar_event_uid,
                                                infil_footprint_ft2 = snapshot$infil_footprint_ft2,
                                                dcia_ft2 = snapshot$dcia_ft2,
                                                orifice_height_ft = snapshot$assumption_orificeheight_ft,
                                                orifice_diam_in = snapshot$orifice_diam_in,
                                                storage_depth_ft = snapshot$storage_depth_ft,
                                                storage_vol_ft3 = snapshot$storage_volume_ft3,
                                                infil_rate_inhr = snapshot$infil_dsg_rate_inhr,
                                                initial_water_level_ft = initial_water_levels$ft,
                                                debug = FALSE)
  
  #short fix
  simulated_data <- simulated_data %>% 
    mutate(radar_event_uid = rainfall_gage_event_uid) %>% 
    dplyr::select(-rainfall_gage_event_uid)
  
  print(paste("Summarizing", sim_candidates$smp_id[i]))
  
  sim_summary <- simulated_data %>%
    dplyr::arrange(dtime_est) %>%
    dplyr::filter(!is.na(radar_event_uid)) %>% #remove rows that had water level data but no event ID
    dplyr::group_by(radar_event_uid) %>%
    dplyr::summarize(
      #Observed storage utilization
      percentstorageused_peak = marsPeakStorage_percent(waterlevel_ft = Simulated_depth_ft, storage_depth_ft = snapshot$storage_depth_ft) %>% round(4),
      
      #Ow uid
      ow_uid = snapshot$ow_uid,
      
      #Observed relative storage utilization
      percentstorageused_relative = marsPeakStorage_percent(waterlevel_ft = Simulated_depth_ft - dplyr::first(Simulated_depth_ft), 
                                                            storage_depth_ft = snapshot$storage_depth_ft) %>% round(4),
      
      #Draindown time
      draindown_hr = marsDraindown_hr(dtime_est = dtime_est,
                                      rainfall_in = rainfall_in,
                                      waterlevel_ft = Simulated_depth_ft),
      
      draindownAssessment = marsDraindownAssessment(level_ft = Simulated_depth_ft,
        eventdepth_in = monitoringdata[["Rain Event Data"]]$eventdepth_in[which(monitoringdata[["Rain Event Data"]]$radar_event_uid == radar_event_uid[1])],
        designdepth_in = snapshot$storage_volume_ft3/snapshot$dcia_ft2*12,
        storage_depth_ft = snapshot$storage_depth_ft,
        draindown_hr = draindown_hr,
        subsurface = TRUE,
        event_id_check = radar_event_uid[1]),
      
      
      
      overtop = marsOvertoppingCheck_bool(Simulated_depth_ft, snapshot$storage_depth_ft),
      
      peakReleaseRate_cfs = marsPeakReleaseRate_cfs(dtime_est, orifice_outflow_ft3 = Simulated_orifice_vol_ft3),
      
      orifice_volume_cf = round(sum((Simulated_orifice_vol_ft3))),
      
      peak_level_ft = max(Simulated_depth_ft),
      
      snapshot_uid = snapshot$snapshot_uid,
      
      observed_simulated_lookup_uid = 2
    )

  depths <- select(monitoringdata[["Rain Event Data"]], radar_event_uid, eventdepth_in)
  
  sim_summary <- left_join(sim_summary, depths)
  
  write_csv(sim_summary, file = paste0(bigfolder, "/", snapshot$smp_id, "_", snapshot$ow_suffix, ".csv"))
  
  for(j in 1:length(sim_summary$radar_event_uid)){
    
    #filter for each event
    selected_event <- obs_data %>%
      dplyr::filter(radar_event_uid == sim_summary$radar_event_uid[j])
    
    rain_plot_data <- monitoringdata[["Rainfall Data"]] %>%
      dplyr::filter(radar_event_uid == sim_summary$radar_event_uid[j])
    
    #only plot rain events greater than 1.5". this can be modified as desired
    if(sim_summary$eventdepth_in[j] > 1){
        sim_event <- simulated_data %>%
          dplyr::filter(radar_event_uid == sim_summary$radar_event_uid[j])
        
        #plot observed and simulated data
        plot <- marsCombinedPlot(event = sim_summary$radar_event_uid[j],
                                 structure_name = paste(sim_candidates$smp_id[i], sim_candidates$ow_suffix),
                                 obs_datetime = selected_event$dtime_est,
                                 obs_level_ft = selected_event$level_ft,
                                 sim_datetime = sim_event$dtime_est,
                                 sim_level_ft = sim_event$Simulated_depth_ft,
                                 storage_depth_ft = snapshot$storage_depth_ft,
                                 orifice_show = TRUE,
                                 orifice_height_ft = snapshot$assumption_orificeheight_ft,
                                 rainfall_datetime = rain_plot_data$dtime_est,
                                 rainfall_in = rain_plot_data$rainfall_in)
        
        ggplot2::ggsave(paste0(folder, "/", paste(sim_candidates$smp_id[i], sim_candidates$ow_suffix[i], sim_summary$radar_event_uid[j], sep = "_"),".png"), plot = plot, width = 10, height = 8)
        
      }
    }
}


metricsfiles <- list.files(path = bigfolder, pattern = "csv$", full.names = TRUE)
metrics <- read_csv(metricsfiles[-134])

write_draindown <- marsWriteDraindownData(con = mars,
                                          draindown_hr = metrics$draindown_hr,
                                          draindown_assessment_lookup_uid = metrics$draindownAssessment,
                                          ow_uid = metrics$ow_uid,
                                          radar_event_uid = metrics$radar_event_uid,
                                          snapshot_uid = metrics$snapshot_uid,
                                          observed_simulated_lookup_uid = metrics$observed_simulated_lookup_uid)

write_overtopping <- marsWriteOvertoppingData(con = mars,
                                              metrics$overtop,
                                              metrics$observed_simulated_lookup_uid,
                                              metrics$ow_uid,
                                              metrics$radar_event_uid,
                                              metrics$snapshot_uid)

write_percent <- marsWritePercentStorageData(mars,
                                             metrics$percentstorageused_peak,
                                             metrics$percentstorageused_relative,
                                             metrics$ow_uid,
                                             metrics$radar_event_uid,
                                             metrics$snapshot_uid,
                                             metrics$observed_simulated_lookup_uid)
