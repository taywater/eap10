## Created by: Brian Cruice
## Created on: 10/01/2021
## Last edit:  11/24/2021
## Purpose: statistical analysis of conclusions made in the 10 Year EAP Narrative


##0.1 library packages and database connections

#library packages
library(DBI)
library(odbc)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(pwdgsi)
library(lme4)
library(outliers)
library(sjPlot)


#database connections
# mars_pg9 <- dbConnect(odbc(),"mars_testing")
mars_pg12 <- dbConnect(odbc(),"mars_data")
tables <- dbListTables(mars_pg12)


##0.2 Rudimentary Queries

#Import SMP/ow data
ow <- dbGetQuery(mars_pg12,"SELECT * FROM fieldwork.ow")

#smp and system metrics, remove retired SMPs and systems

smp_bdv <- dbGetQuery(mars_pg12,"SELECT * FROM external.smpbdv") %>%
           dplyr::filter(is.na(smp_notbuiltretired))

system_bdv <- dbGetQuery(mars_pg12,"SELECT * FROM external.systembdv") %>%
              dplyr::filter(is.na(sys_notbuiltretired))

#Import EAP 10 calc results
snapshot_met <- dbGetQuery(mars_pg12,"SELECT * FROM metrics.snapshot")
overtopping_met <- dbGetQuery(mars_pg12,"SELECT * FROM metrics.overtopping")
percentstorage_met <- dbGetQuery(mars_pg12,"SELECT * FROM metrics.percentstorage")
infil_met <- dbGetQuery(mars_pg12,"SELECT * FROM metrics.infiltration") %>%
             dplyr::filter(is.na(error_lookup_uid) == TRUE)
draindown_met <- dbGetQuery(mars_pg12,"SELECT * FROM metrics.draindown")

long_term_lookup <- dbGetQuery(mars_pg12,"SELECT * FROM fieldwork.long_term_lookup")
long_term_OWs <- dbGetQuery(mars_pg12,"SELECT ow_uid, long_term_lookup_uid FROM fieldwork.deployment WHERE long_term_lookup_uid = 2") %>%
               dplyr::distinct()

#Import EAP 10 calc results (old results)
# snapshot_met <- dbGetQuery(mars_pg12,"SELECT * FROM metrics.snapshot")
# overtopping_met <- dbGetQuery(mars_pg12,"SELECT * FROM metrics.overtopping_oldsims")
# percentstorage_met <- dbGetQuery(mars_pg12,"SELECT * FROM metrics.percentstorage_oldsims")
# infil_met <- dbGetQuery(mars_pg12,"SELECT * FROM metrics.infiltration") %>%
#   dplyr::filter(is.na(error_lookup_uid) == TRUE)
# draindown_met <- dbGetQuery(mars_pg12,"SELECT * FROM metrics.draindown_oldsims")


#lookups
draindown_asmnt <- dbGetQuery(mars_pg12, "SELECT * FROM metrics.draindown_assessment_lookup")
obs_sim_lookup <- dbGetQuery(mars_pg12, "SELECT * FROM metrics.observed_simulated_lookup")

#import rain event data
radar_rain_events <- dbGetQuery(mars_pg12,"SELECT * FROM data.radar_event")

#event depth bins
event_depth_ranges <- c("[0, 0.5)","[0.5, 1.0)","[1, 1.5)","[1.5, 2)","[2, 2.5)","[2.5, 3)", "[3, Inf.)")

radar_rain_events$eventdepth_range[radar_rain_events$eventdepth_in < 0.5] <- event_depth_ranges[1]
radar_rain_events$eventdepth_range[radar_rain_events$eventdepth_in >= 0.5 & radar_rain_events$eventdepth_in < 1.0] <- event_depth_ranges[2]
radar_rain_events$eventdepth_range[radar_rain_events$eventdepth_in >= 1.0 & radar_rain_events$eventdepth_in < 1.5] <- event_depth_ranges[3]
radar_rain_events$eventdepth_range[radar_rain_events$eventdepth_in >= 1.5 & radar_rain_events$eventdepth_in < 2.0] <- event_depth_ranges[4]
radar_rain_events$eventdepth_range[radar_rain_events$eventdepth_in >= 2.0 & radar_rain_events$eventdepth_in < 2.5] <- event_depth_ranges[5]
radar_rain_events$eventdepth_range[radar_rain_events$eventdepth_in >= 2.5 & radar_rain_events$eventdepth_in < 3.0] <- event_depth_ranges[6]
radar_rain_events$eventdepth_range[radar_rain_events$eventdepth_in >= 3.0] <- event_depth_ranges[7]
radar_rain_events$eventdepth_range %<>% as.factor()


#identify surface smp types for separating
surf_smp_types <- c("Rain Garden","Bumpout","Swale","Planter","Basin","Wetland",
                    "Green Roof")

##1.0 GSI Systems Display Excess Storage Capacity Relative to the Design Assumptions

percentstorage_data <- percentstorage_met %>%
      dplyr::left_join(ow, by = "ow_uid") %>%
      dplyr::left_join(radar_rain_events, by = "radar_event_uid") %>%
      dplyr::left_join(snapshot_met, by = "snapshot_uid") %>%
      dplyr::select(-ow_uid.y) %>% dplyr::rename(ow_uid = ow_uid.x) %>%
      dplyr::left_join(obs_sim_lookup, by = "observed_simulated_lookup_uid") %>%
      dplyr::left_join(smp_bdv, by = "smp_id") %>%
      dplyr::filter(!(smp_smptype %in% surf_smp_types)) %>%
      dplyr::mutate(ow_storm = paste0(ow_uid,"-",radar_event_uid)) %>%
      dplyr::filter(relative == 1)

#Review difference in capacity in obs vs. sim
percentstorage_compare <- percentstorage_data %>%
                          dplyr::select(-percentstorage_uid) %>%
                          unique() %>%
                          tidyr::pivot_wider(id_cols = ow_storm,
                                             names_from = type,
                                             values_from = percentstorage) %>%
                          dplyr::filter(is.null(observed)  != TRUE & is.na(observed)  != TRUE) %>%
                          dplyr::filter(is.null(simulated) != TRUE & is.na(simulated) != TRUE) %>%
                          dplyr::mutate(difference = observed - simulated)

overperformance_percent <- (nrow(dplyr::filter(percentstorage_compare, difference < 0))/
                           nrow(percentstorage_compare))*100
underperformance_percent <- (nrow(dplyr::filter(percentstorage_compare, difference > 0))/
                            nrow(percentstorage_compare))*100


##2.0 GSI systems overtop storage capacity substantially less than predicted

overtop_data <- overtopping_met %>%
  dplyr::left_join(ow, by = "ow_uid") %>%
  dplyr::left_join(radar_rain_events, by = "radar_event_uid") %>%
  dplyr::left_join(snapshot_met, by = "snapshot_uid") %>%
  dplyr::left_join(obs_sim_lookup, by = "observed_simulated_lookup_uid") %>%
  dplyr::left_join(smp_bdv, by = "smp_id") %>%
  dplyr::mutate(smp_storm = paste0(smp_id,"-",overtopping_uid)) %>%
  tidyr::pivot_wider(names_from = type, values_from = overtopping)

overtop_data$observed %<>% as.numeric()
overtop_data$simulated %<>% as.numeric()

simulated_overtopping <- overtop_data %>% filter(simulated == 1)
observed_overtopping <- overtop_data %>% filter(observed == 1)

##3.0 Saturated SMP infiltration rates are inelastic to storm size


infil_data <- infil_met %>%
             dplyr::left_join(ow, by = "ow_uid") %>%
             dplyr::left_join(radar_rain_events, by = "radar_event_uid") %>%
             dplyr::left_join(snapshot_met, by = "snapshot_uid") %>%
             dplyr::left_join(obs_sim_lookup, by = "observed_simulated_lookup_uid") %>%
             dplyr::left_join(smp_bdv, by = "smp_id")

obs_infil <- infil_data %>% dplyr::filter(observed_simulated_lookup_uid == 1)

draindown_data <- draindown_met %>%
  dplyr::left_join(ow, by = "ow_uid") %>%
  dplyr::filter(smp_id %in% infil_sub_smps) %>%
  dplyr::left_join(radar_rain_events, by = "radar_event_uid") %>%
  dplyr::left_join(obs_sim_lookup, by = "observed_simulated_lookup_uid")

obs_draindown <- draindown_data %>% dplyr::filter(observed_simulated_lookup_uid == 1)

#remove NA's
obs_infil <- obs_infil[is.na(obs_infil$infiltration_rate_inhr) != TRUE,]
obs_draindown <- obs_draindown[is.na(obs_draindown$draindown_hr) != TRUE,]

#Check for outliers
obs_infil_grubbs <- grubbs.test(obs_infil$infiltration_rate_inhr)
obs_draindown_grubbs <- grubbs.test(obs_draindown$draindown_hr)

#remove outlier
obs_infil <- obs_infil %>% dplyr::filter(infiltration_rate_inhr != max(infiltration_rate_inhr))

#visualize distribution
hist(obs_infil$infiltration_rate_inhr)
hist(log(obs_infil$infil_dsg_rate_inhr))

hist(obs_draindown$draindown_hr)
hist(log(obs_draindown$draindown_hr))

#check normality
shapiro.test(obs_infil$infiltration_rate_inhr)
shapiro.test(log(obs_infil$infiltration_rate_inhr))

shapiro.test(obs_draindown$draindown_hr)
shapiro.test(log(obs_draindown$draindown_hr))

#linear mixed effect model for saturated infiltration as predicted by event depth, random intercepts per smp
infil_mix1 <- lme4::lmer(infiltration_rate_inhr ~ eventdepth_in + (1 | smp_id), obs_infil)
draindown_mix1 <- lme4::lmer(draindown_hr ~ eventdepth_in + (1| smp_id), obs_draindown)
infil_mix1_table <- sjPlot::tab_model(infil_mix1)
draindown_mix1_table <- sjPlot::tab_model(draindown_mix1)


#linear mixed effect model for saturated infiltration as predicted by event depth, random intercepts and slopes per smp
infil_mix2 <- lme4::lmer(infiltration_rate_inhr ~ eventdepth_in + (eventdepth_in | smp_id), obs_infil)
draindown_mix2 <- lme4::lmer(draindown_hr ~ eventdepth_in + (eventdepth_in| smp_id), obs_draindown)

infil_mix2_table <- sjPlot::tab_model(infil_mix2)
draindown_mix2_table <- sjPlot::tab_model(draindown_mix2)


#Summary statistics 
obs_infil %>% group_by(eventdepth_range) %>%
              summarise(mean_obsinfilrate = mean(infiltration_rate_inhr),
                        std_obsinfilrate = sd(infiltration_rate_inhr),
                        observations = n())

obs_draindown %>% group_by(eventdepth_range) %>%
                  summarise(mean_obsdraindown = mean(draindown_hr),
                            std_obsdraindown = sd(draindown_hr),
                            LowerQuartile = quantile(draindown_hr)[2],
                            UpperQuartile = quantile(draindown_hr)[4],
                            observations = n())

quantile(obs_draindown$draindown_hr)

##4.0 Pre-construction infiltration tests routinely under predict SMP recession rates

# Grab statistics, existing plot and conclusions are OK as is

#subsurface, unlined systems
infil_systems <- system_bdv %>%
  dplyr::filter(sys_modelinputcategory == "Subsurface infiltration") %>%
  dplyr::pull(system_id)

#subsurface, unlined smps. Remove surface smps associated with unlined trenches

infil_sub_smps <- smp_bdv %>%
  dplyr::filter(system_id %in% infil_systems) %>%
  dplyr::filter(!(smp_smptype %in% surf_smp_types)) %>%
  dplyr::pull(smp_id)

system_precon_infil <- system_bdv %>% dplyr::select(system_id, infil_constr_rate_inhr, infil_constr_testtype,
                             infil_dsg_rate_inhr,infil_dsg_rate_inhr)


smp_precon_infil <- obs_infil %>%   
                    dplyr::filter(smp_id %in% infil_sub_smps) %>%
                    dplyr::left_join(system_precon_infil, by = "system_id") %>%
                    dplyr::mutate(infil_difference = infiltration_rate_inhr - infil_dsg_rate_inhr)

#Percent of systems where mean observed infiltration rate < pre-con infiltration rate
percent_above_precon <- nrow(dplyr::filter(smp_precon_infil,infil_difference > 0))/nrow(smp_precon_infil)


##5.0 Unlined systems recharge infiltration capacity in roughly constant time irrespective of storm depth


sim_draindown <- draindown_data %>%
                 dplyr::filter(observed_simulated_lookup_uid == 2)

ggplot(draindown_data, aes(x = eventdepth_in, y = draindown_hr, col = type)) +
geom_point(alpha = 0.4) +
  xlab("Event Depth (in)") + ylab("Draindown (hrs)")

under48hr_draindown_percentage <- (1-(sum(obs_draindown$draindown_hr > 48)/nrow(obs_draindown)))*100
over72hr_draindown_percentage <- (sum(obs_draindown$draindown_hr > 72)/nrow(obs_draindown))*100

exceeding_draindown_smps <- obs_draindown %>% dplyr::filter(draindown_hr > 72) %>%
                            dplyr::pull(smp_id)

exceeding_draindown_summary <- obs_draindown %>% dplyr::filter(smp_id %in% exceeding_draindown_smps) %>%
                      group_by(smp_id) %>%
                      summarise(mean_obsdraindown = mean(draindown_hr),
                      std_obsdraindown = sd(draindown_hr),
                      observations = n())

# #normalize data function 
# normalize.data <- function(data_set) {
#   test_results <- shapiro.test(data_set)
#   if(test_results$p.value > 0.05){
#     print("Data set is normally distributed")
#     return(data_set)
#   } else {
#     trans_data <- log(data_set)
#     trans_data <- trans_data[is.na(trans_data) != TRUE]
#     trans_data <- trans_data[is.finite(trans_data) == TRUE]
#     test_results <- shapiro.test(trans_data)
#       if(test_results$p.value > 0.05){
#         print("Data set is normally distributed when log transformed")
#         return(trans_data)
#       } else { print("Data is non-normal.") }
#   }
# }



##6.0 Long-term monitoring sites continue to exceed performance guidelines at Year 10

#create "not in" operator
`%notin%` <- Negate(`%in%`)

#associate long_term_OWs to long term SMPs
long_term_SMPs <- long_term_OWs %>% dplyr::left_join(ow, by = "ow_uid") %>%
                                    dplyr::pull(smp_id)

#list of longterm smps
long_term_smps <- smp_bdv %>% dplyr::filter(smp_id %in% long_term_SMPs)


#long term infiltration and draindown
long_term_draindown <- draindown_data %>% dplyr::filter(smp_id %in% long_term_SMPs) %>%
                                          dplyr::mutate(Year = lubridate::year(eventdatastart_edt))  

long_term_infil <- infil_data %>% dplyr::filter(smp_id %in% long_term_SMPs) %>%
                                  dplyr::mutate(Year = lubridate::year(eventdatastart_edt))



first_storm <- long_term_infil %>% group_by(smp_id) %>%
             dplyr::summarize(first_storm = min(eventdatastart_edt)) %>% unique()

long_term_infil %<>% left_join(first_storm, by = "smp_id") %>%
                     mutate(smp_event_age = time_length(difftime(eventdatastart_edt, first_storm),"years"))
                                  
plot(long_term_infil$smp_event_age, long_term_infil$infiltration_rate_inhr)

long_term_infil_plot <- ggplot(long_term_infil,aes(y = infiltration_rate_inhr, x = smp_event_age, col = eventdepth_range)) +
                        geom_point(alpha = 0.5) + xlab("SMP Age (years)") + ylab("Infiltration Rate (in/hr)") +
                        scale_y_continuous(limits = c(0,10))


#Linear mixed effect model of long term smps
hist(long_term_infil$infiltration_rate_inhr)
long_term_infil$log_infiltration_rate_inhr <- log(long_term_infil$infiltration_rate_inhr)
hist(long_term_infil$log_infiltration_rate_inhr)

long_term_lmer1 <- lme4::lmer(log_infiltration_rate_inhr ~ smp_event_age + (1|smp_id), long_term_infil)
long_term_lmer2 <- lme4::lmer(log_infiltration_rate_inhr ~ smp_event_age + (smp_event_age|smp_id), long_term_infil)
long_term_lmer_table1 <- sjPlot::tab_model(long_term_lmer1)
long_term_lmer_table2 <- sjPlot::tab_model(long_term_lmer2)

#descriptive statistics of long term smps

long_term_smps %>% group_by(smp_smptype) %>% summarise(Count = n())

smp_ages <- long_term_infil %>% dplyr::select(smp_id, first_storm, smp_event_age) %>%
                    dplyr::group_by(smp_id) %>%
                    dplyr::summarise(firststorm = date(first_storm),
                                     system_age_years = max(smp_event_age)) %>% unique()
                    
average_longterm_age <- mean(smp_ages$system_age_years)

##7.0 Surface SMPs routinely recharge within stricter 24-hr drain down window

#systems modeled as surface systems
surface_modelcat <- c("Bioretention (lined)","Bioinfiltration","Bioretention(unlined)")
surface_systems <- system_bdv %>%
                   dplyr::filter(sys_modelinputcategory %in% surface_modelcat) %>%
                   dplyr::pull(system_id) 

surface_smps <- smp_bdv %>%
                dplyr::filter(system_id %in% surface_systems) %>%
                dplyr::pull(smp_id)

#pull draindown data for systems, remove errors
surface_draindown_data <- draindown_met %>%
  dplyr::left_join(ow, by = "ow_uid") %>%
  dplyr::filter(smp_id %in% surface_smps) %>%
  dplyr::left_join(radar_rain_events, by = "radar_event_uid") %>%
  dplyr::left_join(obs_sim_lookup, by = "observed_simulated_lookup_uid") %>%
  dplyr::filter(is.na(error_lookup_uid))

#summary statistics
mean(surface_draindown_data$draindown_hr)
sd(surface_draindown_data$draindown_hr)

