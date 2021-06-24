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

mars <- odbc::dbConnect(odbc::odbc(), "mars_testing")

#font size 
text_size = 22

subsurface_metrics <- dbGetQuery(mars, "select s.ow_uid, s.smp_id, s.ow_suffix, s.asset_type, s.lined, s.surface, s.infiltration_rate_inhr, s.rel_percentstorage, s.draindown_hr, s.dd_assessment_lookup_uid, s.overtopping, s.error_lookup_uid, s.rainfall_radarcell_event_uid, s.eventdepth_in, s.eventpeakintensity_inhr, s.eventdepth_lookup_uid, s.designdepth_in, s.relative_eventdepth_lookup_uid, e.eventdepth_range_in, e.eventdepth_description, r.relative_eventdepth_range_in, r.relative_eventdepth_description, s.observed_simulated_lookup_uid from performance.summary_ow_event_radarcell s left join performance.eventdepth_bin_lookup e on e.eventdepth_lookup_uid = s.eventdepth_lookup_uid left join performance.relative_eventdepth_bin_lookup r on r.relative_eventdepth_lookup_uid = s.relative_eventdepth_lookup_uid where s.surface = false")

#Filter to only those observed events that can also be simulated
#And that have observed responses whose draindown time can be calculated
sub_sim <- subsurface_metrics %>% 
  dplyr::filter(observed_simulated_lookup_uid == 2)

sub_obs <- subsurface_metrics %>% 
  dplyr::filter(observed_simulated_lookup_uid == 1) %>% 
  dplyr::filter(ow_uid %in% sub_sim$ow_uid & rainfall_radarcell_event_uid %in% sub_sim$rainfall_radarcell_event_uid) %>% 
  dplyr::filter(!is.na(draindown_hr))

sub_all <- subsurface_metrics %>% 
  dplyr::filter((ow_uid %in% sub_sim$ow_uid & rainfall_radarcell_event_uid %in% sub_sim$rainfall_radarcell_event_uid) & (ow_uid %in% sub_obs$ow_uid & rainfall_radarcell_event_uid %in% sub_obs$rainfall_radarcell_event_uid)) %>% 
  mutate("Observed" = case_when(observed_simulated_lookup_uid == 1 ~ "Observed", 
                                observed_simulated_lookup_uid == 2 ~ "Simulated"))

#plot draindown by event depth range for smp
draindown_event_depth_plot <-  ggplot(sub_all, 
                                       aes(x = eventdepth_range_in, y = draindown_hr, fill = Observed)) + 
  geom_boxplot() +
  xlab("Event Depth Range (in)") +
  ylab("Draindown (hr)") +
  ggtitle("Draindown times for Subsurface Unlined SMPs") +
  theme(text = element_text(size = text_size))