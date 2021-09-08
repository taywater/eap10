# A LOOP TO TEST pwdgsi ON SUBSURFACE systems
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
mars <- odbc::dbConnect(odbc::odbc(), "mars_data")

#read in surface lined systems 
#make sure you have a good folder path for yourself
ow_testing_list <- read.csv("C:/Users/nicholas.manna/Documents/R/EAP10_nm/csvs for pwdgsi testing/subsurface_unlined_20210217.csv",
                            stringsAsFactors = FALSE) %>%  
  #make column names make sense they can be weird when read in
  dplyr::rename("smp_id" = 1) %>% 
  #only do OW1s for now to avoid weirdness with an ancient CS or GIs
  dplyr::filter(ow_suffix == 'OW1', smp_id == '1-1-1')
#select sites if that's what youre into
 # dplyr::filter(!(smp_id %in% c('1-2-1', '1-3-1', '8-1-1', '9-1-1', '9-2-1', '88-1-1', 
 #                             '170-1-1', '170-2-1', '187-3-3', '250-1-1', '250-2-1', '250-3-1', '326-1-1'))) %>% 
  #dplyr::filter(ow_suffix == 'OW1')

#create a stamp function, so date inputs fit the standard for pwdgsi functions #not sure how necessary this is
sf <- lubridate::stamp("2014-12-31")

#initialize an empty list, the length of ow_lesting_list
sum_list <- vector(mode = "list", length = length(ow_testing_list$start_date))

#for(i in 1:length(ow_testing_list$start_date)){
i <- 1
  # top of fn----------
  #read dates and SMPs from testing list
  #start_date <- sf(mdy(ow_testing_list$start_date[i]))#, sf(mdy(ow_testing_list$start_date[i+1])))
  start_date <- "2019-12-01"
  #end_date   <- sf(mdy(ow_testing_list$end_date[i]))#, sf(mdy(ow_testing_list$end_date[i+1])))
  end_date <- "2020-02-01"
  target_id  <- ow_testing_list$smp_id[i] %>% as.character()#, ow_testing_list$smp_id[i+1] %>% as.character())
  ow_suffix  <- ow_testing_list$ow_suffix[i] %>% as.character()#, ow_testing_list$ow_suffix[i] %>% as.character())
  
  #call snapshot
  snapshot <- marsFetchSMPSnapshot(con = mars, 
                                   smp_id = target_id, 
                                   ow_suffix = ow_suffix, 
                                   request_date = "today")

  #check if simulation should happen, based on available snapshot fields
  #right now it will not happen so i comment out
  sim_true <- !(is.na(snapshot$storage_depth_ft)| is.na(snapshot$dcia_ft2) | is.na(snapshot$storage_volume_ft3) |
                  snapshot$storage_depth_ft == 0 | snapshot$dcia_ft2 == 0 | snapshot$storage_volume_ft3 == 0 |
                  (snapshot$infil_dsg_rate_inhr == 0 & is.na(snapshot$orifice_diam_in)) |
                  (is.na(snapshot$infil_dsg_rate_inhr) & is.na(snapshot$orifice_diam_in)) |
                  ((snapshot$infil_footprint_ft2 == 0 | is.na(snapshot$infil_footprint_ft)) & is.na(snapshot$orifice_diam_in)))
  
  #set sim_true to FALSE to skip simulations (comment out if you want to do simulations)
  #keep this here because some conditionals rely on it
  # sim_true <- FALSE
  
  #fetch monitoring data
  monitoringdata <- marsFetchMonitoringData(con = mars, 
                                            target_id = target_id, 
                                            ow_suffix = ow_suffix, 
                                            source = "radar",
                                            start_date = start_date, 
                                            end_date = end_date, 
                                            sump_correct = TRUE,
                                            debug = TRUE)
  
  rain_event_data <- monitoringdata[["Rain Event Data"]]
  rain_data <- monitoringdata[["Rainfall Data"]]
  level_data <- monitoringdata[["Level Data"]]
  
  #print i and smp_id so we know the loop progress (and where it is if it breaks)
  print(i)
  print(target_id)
  
  #only analyze locations with events
  if(length(rain_event_data$radar_event_uid) > 0){
    #create folders for plots and plots with errors
    folder <- (paste0("O:/Watershed Sciences/GSI Monitoring/06 Special Projects/34 PWDGSI metrics calculations/EAP10/20210908_pg12_testing/", paste(target_id, ow_suffix, sep = "_")))
    error_folder = paste0(folder, "/error")
    dir.create(folder, showWarnings = FALSE)
    dir.create(error_folder, showWarnings = FALSE)
    
    #join monitoring data in one table
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
      dplyr::summarize(
        ft = dplyr::first(level_ft)
      )
    
    initial_water_levels <- initial_water_levels[complete.cases(initial_water_levels), ]
    

    #observed ---- 
    
    #create a summary table of observed data
    observed_summary <- obs_data %>%
      dplyr::arrange(dtime_est) %>% 
      dplyr::filter(is.na(radar_event_uid) == FALSE) %>% #remove rows that had water level data but no event ID
      #dplyr::filter(radar_event_uid == 159691) %>% 
      dplyr::group_by(radar_event_uid) %>%
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
        
        infiltration_inhr = marsInfiltrationRate_inhr(event = radar_event_uid,
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
                                                      eventdepth_in = rain_event_data$eventdepth_in[which(rain_event_data$radar_event_uid == radar_event_uid[1])], 
                                                      designdepth_in = snapshot$storage_volume_ft3/snapshot$dcia_ft2*12, 
                                                      storage_depth_ft = snapshot$storage_depth_ft, 
                                                      draindown_hr = draindown_hr, 
                                                      subsurface = TRUE, 
                                                      event_id_check = radar_event_uid[1]), 
        
        overtop = marsOvertoppingCheck_bool(level_ft, snapshot$storage_depth_ft),
        
        peakReleaseRate_cfs = marsPeakReleaseRate_cfs(dtime_est, orifice_outflow_ft3 = orifice_outflow_ft3),
        
        orifice_volume_cf = round(sum(orifice_outflow_ft3),0),
        
        snapshot_uid = snapshot$snapshot_uid,
        
        observed_simulated_lookup_uid = 1
      )
    
    #combined_observed_summary <- bind_rows(combined_observed_summary, observed_summary)
    
    #sim  ---
    if(sim_true){
      ptm <- proc.time()
      simulated_data <- marsSimulatedLevelSeries_ft(dtime_est = rain_data$dtime_est,
                                                    rainfall_in = rain_data$rainfall_in,
                                                    event = rain_data$radar_event_uid,
                                                    infil_footprint_ft2 = snapshot$infil_footprint_ft2,
                                                    dcia_ft2 = snapshot$dcia_ft2,
                                                    orifice_height_ft = snapshot$assumption_orificeheight_ft,
                                                    orifice_diam_in = snapshot$orifice_diam_in,
                                                    storage_depth_ft = snapshot$storage_depth_ft,
                                                    storage_vol_ft3 = snapshot$storage_volume_ft3,
                                                    infil_rate_inhr = snapshot$infil_dsg_rate_inhr,
                                                    initial_water_level_ft = initial_water_levels$ft,
                                                    debug = FALSE)
      proc.time() - ptm
      
      #short fix
      simulated_data <- simulated_data %>% 
        mutate(rainfall_radarcell_event_uid = rainfall_gage_event_uid) %>% 
        dplyr::select(-rainfall_gage_event_uid)
    }
    
    
    
    #create simulated summary
    if(sim_true){
      sim_summary <- simulated_data %>%
        dplyr::arrange(dtime_est) %>%
        #remove rows that had water level data but no event ID
        dplyr::filter(is.na(rainfall_radarcell_event_uid) == FALSE & rainfall_radarcell_event_uid) %>%
        # dplyr::filter(rainfall_radarcell_event_uid == 121434) %>% 
        #dplyr::filter(is.na(rainfall_gage_event_uid) == FALSE & rainfall_gage_event_uid) %>% #remove rows that had water level data but no event ID
        dplyr::group_by(rainfall_radarcell_event_uid) %>%
        #dplyr::group_by(rainfall_gage_event_uid) %>%
        dplyr::summarize(
          
          #Observed storage utilization
          percentstorageused_peak = marsPeakStorage_percent(waterlevel_ft = Simulated_depth_ft, storage_depth_ft = snapshot$storage_depth_ft) %>% round(4),
          
          #Ow uid
          ow_uid = snapshot$ow_uid,
          
          #first_point = dplyr::first(Simulated_depth_ft),
          
          #Observed relative storage utilization
          percentstorageused_relative = marsPeakStorage_percent(waterlevel_ft = Simulated_depth_ft - dplyr::first(Simulated_depth_ft), storage_depth_ft = snapshot$storage_depth_ft) %>% round(4),
          
          #Draindown time
          draindown_hr = marsDraindown_hr(dtime_est = dtime_est,
                                          rainfall_in = rainfall_in,
                                          waterlevel_ft = Simulated_depth_ft),
          
          draindownAssessment = marsDraindownAssessment(level_ft = Simulated_depth_ft,
                                                        eventdepth_in = rain_event_data$eventdepth_in[which(rain_event_data$rainfall_radarcell_event_uid == rainfall_radarcell_event_uid[1])],
                                                        designdepth_in = snapshot$storage_volume_ft3/snapshot$dcia_ft2*12,
                                                        storage_depth_ft = snapshot$storage_depth_ft,
                                                        draindown_hr = draindown_hr,
                                                        subsurface = TRUE,
                                                        event_id_check = rainfall_radarcell_event_uid[1]),
          
          overtop = marsOvertoppingCheck_bool(Simulated_depth_ft, snapshot$storage_depth_ft),
          
          peakReleaseRate_cfs = marsPeakReleaseRate_cfs(dtime_est, orifice_outflow_ft3 = Simulated_orifice_vol_ft3),
          
          orifice_volume_cf = round(sum((Simulated_orifice_vol_ft3))),
          
          peak_level_ft = max(Simulated_depth_ft),
          
          snapshot_uid = snapshot$snapshot_uid,
          
          observed_simulated_lookup_uid = 2
        )
    }else{
      print("snapshot contains NAs")
    }
    
    #other stuff ----
    
    #merge observed and simulated summaries if necessary
    if(sim_true){
      obs_sim_summary <- dplyr::bind_rows(observed_summary, sim_summary)
    }else{
      obs_sim_summary <- observed_summary
    }
    
    #write csv of observed summary
    write.csv(observed_summary, paste0(folder, "/", target_id, "_", ow_suffix, "summary.csv"))
    
    #join observed summary to rain
    rain_observed_summary <- left_join(observed_summary, rain_event_data, by = "radar_event_uid")
    
    #write csv of observed summary with rain
    write.csv(rain_observed_summary, paste0(folder, "/", target_id, "_", ow_suffix, "rain_summary.csv"))
    ######
    #plots!
   for(j in 1:length(rain_event_data$radar_event_uid)){
     #j <-1
     #filter for each event
       selected_event <- obs_data %>%
         dplyr::filter(radar_event_uid == rain_event_data$radar_event_uid[j])
       rain_plot_data <- monitoringdata[["Rainfall Data"]] %>%
         dplyr::filter(radar_event_uid == rain_event_data$radar_event_uid[j])

       #only plot rain events greater than 0.5". this can be modified as desired
       if(rain_event_data$eventdepth_in[j] > 0.5){
       #skip plots with errors -900
       #if simulation ran, plot sim and obs. if not, just plot observed
       if(sim_true & obs_sim_summary$infiltration_inhr[j] != -900){
         sim_event <- simulated_data %>%
           dplyr::filter(radar_event_uid == rain_event_data$radar_event_uid[j])
         #plot observed and simulated data
         plot <- marsCombinedPlot(event = rain_event_data$radar_event_uid[j],
                                  structure_name = paste(target_id, ow_suffix),
                                  obs_datetime = selected_event$dtime_est,
                                  obs_level_ft = selected_event$level_ft,
                                  sim_datetime = sim_event$dtime_est,
                                  sim_level_ft = sim_event$Simulated_depth_ft,
                                  storage_depth_ft = snapshot$storage_depth_ft,
                                  orifice_show = TRUE,
                                  orifice_height_ft = snapshot$assumption_orificeheight_ft,
                                  rainfall_datetime = rain_plot_data$dtime_est,
                                  rainfall_in = rain_plot_data$rainfall_in)
       }else if(obs_sim_summary$infiltration_inhr[j] != -900){
           #   # plot observed data
           plot <- marsCombinedPlot(event = rain_event_data$radar_event_uid[j],
                                    structure_name = paste(target_id, ow_suffix),
                                    obs_datetime = selected_event$dtime_est,
                                    obs_level_ft = selected_event$level_ft,
                                    storage_depth_ft = snapshot$storage_depth_ft,
                                    orifice_show = TRUE,
                                    orifice_height_ft = snapshot$assumption_orificeheight_ft,
                                    rainfall_datetime = rain_plot_data$dtime_est,
                                    rainfall_in = rain_plot_data$rainfall_in)

         # }
         #save plot
         #put errors in a different folder
         #if(obs_sim_summary$infiltration_inhr[j] > -900){
           ggplot2::ggsave(paste0(folder, "/", paste(target_id, ow_suffix, rain_event_data$radar_event_uid[j], sep = "_"),".png"), plot = plot, width = 10, height = 8)
         #}else if(obs_sim_summary$infiltration_inhr[j] != -900){
        #   ggplot2::ggsave(paste0(error_folder, "/", paste(target_id, ow_suffix, rain_event_data$radar_event_uid[j], sep = "_"),".png"), plot = plot, width = 10, height = 8)
         }
       }
   }
  
  
  #writing to database
  #this has been excluded from the loops, for now
  # this can be commented if needed
  write_draindown <- marsWriteDraindownData(con = mars,
                                            draindown_hr = obs_sim_summary$draindown_hr,
                                            draindown_assessment_lookup_uid = obs_sim_summary$draindownAssessment,
                                            ow_uid = obs_sim_summary$ow_uid,
                                            radar_event_uid =obs_sim_summary$radar_event_uid, 
                                            snapshot_uid = obs_sim_summary$snapshot_uid,
                                            observed_simulated_lookup_uid =obs_sim_summary$observed_simulated_lookup_uid)
  
  con <- mars
  draindown_hr <- obs_sim_summary$draindown_hr
  draindown_assessment_lookup_uid <- obs_sim_summary$draindownAssessment
  ow_uid <- obs_sim_summary$ow_uid
  radar_event_uid <-obs_sim_summary$radar_event_uid 
  snapshot_uid <- obs_sim_summary$snapshot_uid
  observed_simulated_lookup_uid <-obs_sim_summary$observed_simulated_lookup_uid
  
  
  if(!(length(draindown_hr) == length(ow_uid) &
       length(draindown_hr) == length(draindown_assessment_lookup_uid) &
       length(draindown_hr) == length(radar_event_uid) &
       length(draindown_hr) == length(snapshot_uid) &
       length(draindown_hr) == length(observed_simulated_lookup_uid))){
    stop("Vectors must be the same length")
  }  
  
  #add vectors to dataframe
  draindown_df <- data.frame(draindown_hr,
                             observed_simulated_lookup_uid,
                             ow_uid,
                             radar_event_uid,
                             snapshot_uid, 
                             draindown_assessment_lookup_uid)
  
  #select columns for dataframe
  draindown_df <- draindown_df %>% 
    dplyr::mutate("error_lookup_uid" = ifelse(draindown_hr <0,
                                              draindown_hr, NA),
                  draindown_hr = ifelse(!is.na(error_lookup_uid),
                                        NA, draindown_hr))
  
  #write to table, and return either TRUE (for a succesful write) or the error (upon failure)
  result <- tryCatch(odbc::dbWriteTable(con, DBI::SQL("metrics.draindown"), draindown_df, overwrite = FALSE, append = TRUE), 
                     error = function(error_message){
                       return(error_message$message)
                     })
  
  
  
  write_infiltration <- marsWriteInfiltrationData(mars, 
                                                  obs_sim_summary$infiltration_inhr,
                                                  obs_sim_summary$baseline,
                                                  obs_sim_summary$ow_uid,
                                                  obs_sim_summary$radar_event_uid, 
                                                  obs_sim_summary$snapshot_uid,
                                                  obs_sim_summary$observed_simulated_lookup_uid)
  
  write_overtopping <- marsWriteOvertoppingData(mars,
                                                obs_sim_summary$overtop,
                                                obs_sim_summary$observed_simulated_lookup_uid,
                                                obs_sim_summary$ow_uid,
                                                obs_sim_summary$radar_event_uid,
                                                obs_sim_summary$snapshot_uid)
  
  write_percent <- marsWritePercentStorageData(mars,
                                               obs_sim_summary$percentstorageused_peak,
                                               obs_sim_summary$percentstorageused_relative,
                                               obs_sim_summary$ow_uid,
                                               obs_sim_summary$radar_event_uid,
                                               obs_sim_summary$snapshot_uid,
                                               obs_sim_summary$observed_simulated_lookup_uid)
  
  }
}

#copy this chunk above the observed summary pipeline when we want to sim
#simulate ----
# if(sim_true){
#   ptm <- proc.time()
#   simulated_data <- marsSimulatedLevelSeries_ft(dtime_est = rain_data$dtime_est,
#                                                 rainfall_in = rain_data$rainfall_in,
#                                                 event = rain_data$rainfall_radarcell_event_uid,
#                                                 infil_footprint_ft2 = snapshot$infil_footprint_ft2,
#                                                 dcia_ft2 = snapshot$dcia_ft2,
#                                                 orifice_height_ft = snapshot$assumption_orificeheight_ft,
#                                                 orifice_diam_in = snapshot$orifice_diam_in,
#                                                 storage_depth_ft = snapshot$storage_depth_ft,
#                                                 storage_vol_ft3 = snapshot$storage_volume_ft3,
#                                                 infil_rate_inhr = snapshot$infil_dsg_rate_inhr, 
#                                                 initial_water_level_ft = initial_water_levels$ft,
#                                                 debug = FALSE)
#   proc.time() - ptm
# }
# 
# #create simulated summary
# if(sim_true){
#   sim_summary <- simulated_data %>%
#     dplyr::arrange(dtime_est) %>%
#     dplyr::filter(is.na(rainfall_radarcell_event_uid) == FALSE & rainfall_radarcell_event_uid) %>% #remove rows that had water level data but no event ID
#     dplyr::group_by(rainfall_radarcell_event_uid) %>%
#     dplyr::summarize(
#       
#       #Observed storage utilization
#       percentstorageused_peak = marsPeakStorage_percent(waterlevel_ft = Simulated_depth_ft, storage_depth_ft = snapshot$storage_depth_ft) %>% round(4),
#       
#       #Ow uid
#       ow_uid = snapshot$ow_uid,
#       
#       #first_point = dplyr::first(Simulated_depth_ft), 
#       
#       #Observed relative storage utilization
#       percentstorageused_relative = marsPeakStorage_percent(waterlevel_ft = Simulated_depth_ft - dplyr::first(Simulated_depth_ft), storage_depth_ft = snapshot$storage_depth_ft) %>% round(4),
#       
#       #Draindown time
#       draindown_hr = marsDraindown_hr(dtime_est = dtime_est,
#                                       rainfall_in = rainfall_in,
#                                       waterlevel_ft = Simulated_depth_ft),
#       
#       overtop = marsOvertoppingCheck_bool(Simulated_depth_ft, snapshot$storage_depth_ft),
#       
#       peakReleaseRate_cfs = marsPeakReleaseRate_cfs(dtime_est, orifice_outflow_ft3 = Simulated_orifice_vol_ft3),
#       
#       orifice_volume_cf = round(sum((Simulated_orifice_vol_ft3))),
#       
#       snapshot_uid = snapshot$snapshot_uid,
#       
#       observed_simulated_lookup_uid = 2
#     )
# }else{
#   print("snapshot contains NAs")
# }


  

