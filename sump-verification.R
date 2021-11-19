library(pwdgsi)
library(odbc)
library(tidyverse)
library(lubridate)

mars <- dbConnect(odbc(), "mars_data")

accessdb <- dbGetQuery(mars, "select a.ow_uid, a.filepath, o.smp_id, o.ow_suffix from admin.accessdb a left join fieldwork.ow o on a.ow_uid = o.ow_uid where a.sumptable is null and a.filepath is not null and o.smp_id like '%-%-%'")

calcs <- dbGetQuery(mars, "with oops_all_uids as (select distinct ow_uid from metrics.percentstorage union select distinct ow_uid from metrics.draindown union select distinct ow_uid from metrics.overtopping union select distinct ow_uid from metrics.infiltration) select distinct ow_uid from oops_all_uids order by ow_uid asc")

calcced_systems <- filter(accessdb, ow_uid %in% calcs$ow_uid)

dir.create("//pwdoows/oows/Watershed Sciences/GSI Monitoring/06 Special Projects/34 PWDGSI metrics calculations/EAP10/20211102", showWarnings = FALSE)
dir.create("//pwdoows/oows/Watershed Sciences/GSI Monitoring/06 Special Projects/34 PWDGSI metrics calculations/EAP10/20211102/timeseries", showWarnings = FALSE)

for(i in 1:nrow(calcced_systems)){
  site_data <- dbGetQuery(mars, paste("select * from data.ow_leveldata_raw where ow_uid =", calcced_systems$ow_uid[i], "order by dtime_est asc"))
  
  site_plot <- ggplot(data = site_data) + 
    geom_line(aes(x = dtime_est, y = level_ft), size = 1.5) + 
    scale_y_continuous(breaks = seq(0, 5, by = 0.25)) + 
    geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    labs(x = NULL, y = "Water Level (ft)", title = paste0(calcced_systems$smp_id[i], " " , calcced_systems$ow_suffix[i], " Water Level Time Series (ow_uid = ", calcced_systems$ow_uid[i], ")"))
  
  ggsave(filename = paste0("//pwdoows/oows/Watershed Sciences/GSI Monitoring/06 Special Projects/34 PWDGSI metrics calculations/EAP10/20211102/timeseries/", calcced_systems$smp_id[i], " " , calcced_systems$ow_suffix[i], ".png"), site_plot, width = 8, height = 6, units = "in")
}
