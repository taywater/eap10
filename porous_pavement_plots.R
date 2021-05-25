#plot porous pavement infiltration / pass through rates compared to restorative maintenance
#5/19/21

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


date <- "20210524/"

#font size 
text_size = 22

#folder organization
type <- "porous_pavement/"

# 1.0 queries and plots ------

#1.1 data -----
#porous pavement data

pp_data <- odbc::dbGetQuery(mars, "select * from fieldwork.porous_pavement_average_rates")

maint_data <- dbGetQuery(mars, "select * from fieldwork.porous_pavement_maintained_sites_experiment") %>% 
  dplyr::filter(!is.na(avg_rate_inhr)) %>% 
  mutate(test_date = ymd_hms(paste(as.character(test_date), "00:00:01"))) %>% 
  mutate(site_name = case_when(smp_id == "207-1-3" ~ "Waterview", 
                               smp_id == "240-1-1" ~ "Percy St.", 
                               smp_id == "288-1-1" ~ "Webster", 
                               smp_id == "301-1-1" ~ "Collins St.", 
                               smp_id == "301-3-1" ~ "Gordon St.", 
                               smp_id == "329-1-1" ~ "Hope St., Master to Jefferson", 
                               smp_id == "331-1-1" ~ "Hope St., Berks to Norris"))

maintenance <- dbGetQuery(mars, "select * from fieldwork.porous_pavement_maintenance")

some_waterleveldata <- dbGetQuery(mars, "select * from ow_leveldata_raw limit 10")

#1.2 plotting

maint_plot <- ggplot(maint_data, aes(x = test_date, y = avg_rate_inhr, color = site_name)) +
  geom_line(size = 1.1, show.legend = TRUE) +
  geom_point(size = 2, show.legend = TRUE) +
  scale_y_continuous(trans = 'log10') +
  coord_cartesian(xlim = c(ymd_hms("2018-12-01 00:00:01"), ymd_hms("2019-09-06 00:00:01")), 
                  ylim = c(1, 100)) +
  geom_vline(aes(xintercept = ymd_hms("2019-04-30 00:00:01"))) + 
  geom_vline(aes(xintercept = ymd_hms("2019-08-20 00:00:01"))) +
  annotate("label", 
           x = ymd_hms("2019-06-24 00:00:0"), 
           y = 85, 
           label = "< Maintenance Events >", 
           size = text_size/2.5) +
  xlab("Date") + 
  ylab("Average Infiltration Rate (in/hr)") +
  labs(color = "SMP ID") +
  ggtitle("ROW Porous Pavement Infiltration Rates (2019)") +
  theme(text = element_text(size = text_size), legend.position = "bottom")# + 
  #guides(color = guide_legend(nrow = 10))

  
  #scale_x_datetime(limits = c(ymd_hms("2019-01-01 00:00:01"), ymd_hms("2020-04-04 00:00:01")))

maint_plot

ggsave(filename = paste0(folder,date, type, "porous_pavement_maintenance.jpg"), plot = maint_plot, width = 11, height = 8, units = "in")


