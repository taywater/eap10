## Created by: Brian Cruice
## Created on: 10/01/2021
## Purpose: statistical analysis of conclusions made in the 10 Year EAP Narrative


##0.1 library packages and database connections

#library packages
library(DBI)
library(odbc)
library(tidyverse)
library(ggplot2)
library(pwdgsi)
library(lme4)
library(outliers)
library(sjPlot)


#database connections
# mars_pg9 <- dbConnect(odbc(),"mars_testing")
mars_pg12 <- dbConnect(odbc(),"mars_data")
dbListTables(mars_pg12)


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

#lookups
draindown_asmnt <- dbGetQuery(mars_pg12, "SELECT * FROM metrics.draindown_assessment_lookup")
obs_sim_lookup <- dbGetQuery(mars_pg12, "SELECT * FROM metrics.observed_simulated_lookup")

#import rain event data
radar_rain_events <- dbGetQuery(mars_pg12,"SELECT * FROM data.radar_event")

##1.0 GSI Systems Display Excess Storage Capacity Relative to the Design Assumptions



##2.0 GSI systems overtop storage capacity substantially less than predicted

overtop_data <- overtopping_met %>%
  dplyr::left_join(ow, by = "ow_uid") %>%
  dplyr::left_join(radar_rain_events, by = "radar_event_uid") %>%
  dplyr::left_join(snapshot_met, by = "snapshot_uid") %>%
  dplyr::left_join(obs_sim_lookup, by = "observed_simulated_lookup_uid") %>%
  dplyr::left_join(smp_bdv, by = "smp_id") %>%
  dplyr::mutate(smp_storm = paste0(smp_id,overtopping_uid,sep = "-")) %>%
  tidyr::pivot_wider(names_from = type, values_from = overtopping)

overtop_data$observed %<>% as.numeric()
overtop_data$simulated %<>% as.numeric()

simulated_overtopping <- overtop_data %>% filter(simulated == 1)
observed_overotpping <- overtop_data %>% filter(observed == 1)

##3.0 Saturated SMP infiltration rates are inelastic to storm size


infil_data <- infil_met %>%
             dplyr::left_join(ow, by = "ow_uid") %>%
             dplyr::left_join(radar_rain_events, by = "radar_event_uid") %>%
             dplyr::left_join(snapshot_met, by = "snapshot_uid") %>%
             dplyr::left_join(obs_sim_lookup, by = "observed_simulated_lookup_uid") %>%
             dplyr::left_join(smp_bdv, by = "smp_id") %>%



obs_infil <- infil_data %>% dplyr::filter(observed_simulated_lookup_uid == 1)

#remove NA's
obs_infil <- obs_infil[is.na(obs_infil$infiltration_rate_inhr) != TRUE,]

#Check for outliers
obs_infil_grubbs <- grubbs.test(obs_infil$infiltration_rate_inhr)
#remove outlier
obs_infil <- obs_infil %>% dplyr::filter(infiltration_rate_inhr != max(infiltration_rate_inhr))

#visualize distribution
hist(obs_infil$infiltration_rate_inhr)
hist(log(obs_infil$infil_dsg_rate_inhr))

#check normality
shapiro.test(obs_infil$infiltration_rate_inhr)
shapiro.test(obs_infil$)

#linear mixed effect model for saturated infiltration as predicted by event depth, random intercepts per smp
infil_mix1 <- lme4::lmer(infiltration_rate_inhr ~ eventdepth_in + (1 | smp_id), obs_infil)

#linear mixed effect model for saturated infiltration as predicted by event depth, random intercepts and slopes per smp
infil_mix2 <- lme4::lmer(infiltration_rate_inhr ~ eventdepth_in + (eventdepth_in | smp_id), obs_infil)

##4.0 Pre-construction infiltration tests routinely under predict SMP recession rates



##5.0 Unlined systems recharge infiltration capacity in roughly constant time irrespective of storm depth

#subsurface, unlined systems
infil_systems <- system_bdv %>%
                 dplyr::filter(sys_modelinputcategory == "Subsurface infiltration") %>%
                 dplyr::pull(system_id)

#subsurface, unlined smps. Remove surface smps associated with unlined trenches
surf_smp_types <- c("Rain Garden","Bumpout","Swale","Planter","Basin","Wetland",
                    "Green Roof")

infil_sub_SMPs <- smp_bdv %>%
                  dplyr::filter(system_id %in% infil_systems) %>%
                  dplyr::filter(!(smp_smptype %in% surf_smp_types)) %>%
                  dplyr::pull(smp_id)

draindown_data <- draindown_met %>%
                 dplyr::left_join(ow, by = "ow_uid") %>%
                 dplyr::filter(smp_id %in% infil_sub_SMPs) %>%
                 dplyr::left_join(radar_rain_events, by = "radar_event_uid") %>%
                 dplyr::left_join(obs_sim_lookup, by = "observed_simulated_lookup_uid")

obs_draindown <- draindown_data %>%
                 dplyr::filter(observed_simulated_lookup_uid == 1)
sim_draindown <- draindown_data %>%
                 dplyr::filter(observed_simulated_lookup_uid == 2)

ggplot(draindown_data, aes(x = eventdepth_in, y = draindown_hr, col = type)) +
geom_point(alpha = 0.4) +
  xlab("Event Depth (in)") + ylab("Draindown (hrs)")

hist(log(obs_draindown$draindown_hr))

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



##7.0 Surface SMPs routinely recharge within stricter 24-hr draindown window