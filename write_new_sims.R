library(odbc)
library(lubridate)
library(tidyverse)

hostfolder <- "//pwdoows/oows/Watershed Sciences/GSI Monitoring/06 Special Projects/34 PWDGSI metrics calculations/EAP10/20211130_newsims"

mars <- dbConnect(odbc(), "mars_data")

sim_results <- list.files(hostfolder, pattern = "*.csv", full.names=TRUE)

sim_data <- data.frame(NULL)

for(i in 1:length(sim_results)){
  sim_sheet <- read_csv(sim_results[i])
  sim_data <- bind_rows(sim_data, sim_sheet)
}

draindown <- transmute(sim_data, draindown_hr, observed_simulated_lookup_uid, ow_uid, radar_event_uid, snapshot_uid, draindown_assessment_lookup_uid = draindownAssessment, error_lookup_uid = draindown_hr)
draindown$error_lookup_uid[draindown$draindown_hr >= 0] <- NA
draindown$draindown_hr[draindown$draindown_hr < 0] <- NA

overtopping <- select(sim_data, overtopping = overtop, observed_simulated_lookup_uid, ow_uid, radar_event_uid, snapshot_uid)

percentstorage_peak <- transmute(sim_data, percentstorage = percentstorageused_peak, relative = FALSE, observed_simulated_lookup_uid, ow_uid, radar_event_uid, snapshot_uid)
percentstorage_rel <- transmute(sim_data, percentstorage = percentstorageused_relative, relative = TRUE, observed_simulated_lookup_uid, ow_uid, radar_event_uid, snapshot_uid)
percentstorage <- bind_rows(percentstorage_peak, percentstorage_rel)


dbWriteTable(mars, DBI::SQL("metrics.draindown"), draindown, append = TRUE)

dbWriteTable(mars, DBI::SQL("metrics.overtopping"), overtopping, append = TRUE)

dbWriteTable(mars, DBI::SQL("metrics.percentstorage"), percentstorage, append = TRUE)
