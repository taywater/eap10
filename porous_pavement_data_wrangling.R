#getting data from various sources into mars_testing
#for porous pavement
#will need to join by date and smp id and test location to get porous_pavement_uid and then that will be the joining key in the future
#nick manna
#5/17/21

#0.0 set up -----

#libraries
library(odbc)
library(tidyverse)
library(readxl)
library(lubridate)

#db connection
mars <- odbc::dbConnect(odbc::odbc(), "mars_testing")

#1.0 get data -------

#fieldwork.porous_pavement - porous pavement records
fpp <- odbc::dbGetQuery(mars, "select * from fieldwork.porous_pavement")

#porous pavement raw data
raw <- read_excel("C:/Users/nicholas.manna/Documents/R/working/porous/porous_pavement_raw_access.xlsx") %>% select(-1)

#porous pavement results data
results <- read_excel("C:/Users/nicholas.manna/Documents/R/working/porous/porous_pavement_results_access.xlsx") %>% select(-1)


#1.1 flatten dates

raw <- raw %>% mutate("date" = as.Date(date))

results <- results %>% mutate("date" = as.Date(date))

#pivot longer - raw data get a row for each test
# raw_long <- raw %>% 
#   pivot_longer(cols = c(test_one_weight_lbs, test_two_weight_lbs, test_one_time_s, test_two_time_s), 
#                names_to = c("set", ".value"), 
#                names_pattern = "()()")

#1.2 pivot longer and clean up data
#it was too hard to pivot_longer with two sets of columns so i did one and then the other and filtered
raw_long_one <- raw %>% 
  pivot_longer(cols = c(test_one_weight_lbs, test_two_weight_lbs), 
               names_to = c("set"))

raw_long_two <- raw_long_one %>% 
  pivot_longer(cols = c(test_one_time_s, test_two_time_s), 
               names_repair = "unique")

raw_long <- raw_long_two %>% 
  dplyr::filter((set == "test_one_weight_lbs" & name == "test_one_time_s") | (set == "test_two_weight_lbs" & name == "test_two_time_s")) %>% 
  dplyr::rename("weight_lb" = 7, "time_s" = 9) %>% 
  dplyr::select(-set) %>% 
  tidyr::separate(name, c("first", "name", "third", "fourth"), sep = "_") %>% select(-first, -third, -fourth)

results_long <- results %>% 
 pivot_longer(cols = c(test_one_inhr, test_two_inhr)) %>% 
  tidyr::separate(name, c("first", "name", "third"), sep = "_") %>% select(-first, -third)

#1.3 joins
#join raw and results
raw_and_results <- raw_long %>% 
  left_join(results_long, by = c("date", "smp_id", "test_location", "name"))

#join results and porous pavement records
#clean data
#remove records where there were too many sets of tests on day and the joins got mixed up 
#we can revisit these later or just...not. most are 445-1-1
fpp_raw_results <- fpp %>% 
  left_join(raw_and_results, by = c("test_date" = "date", "smp_id", "test_location")) %>%
  select(-avg_inhr, -notes, -prewet_time_min.x, -name) %>%
  rename("prewet_time_min" = prewet_time_min.y, "rate_inhr" = value, "weight_lbs" = weight_lb) %>% 
  mutate("prewet_time_s" = case_when(smp_id == "445-1-1" ~ 25, 
                                   TRUE ~ prewet_time_min)) %>% 
  dplyr::select(-prewet_time_min) %>% 
  distinct %>% 
  dplyr::filter(!(porous_pavement_uid %in% c(264, 265, 266, 268, 270, 271, 272, 274, 275, 276, 261)))

#select the columns to send to fieldwork.porous_pavement_results
fppr <- fpp_raw_results %>% 
  dplyr::select(porous_pavement_uid, weight_lbs, time_s, rate_inhr)

# for(i in 1:length(fppr$porous_pavement_uid)){
# 
# dbGetQuery(mars, paste("insert into fieldwork.porous_pavement_results(porous_pavement_uid, weight_lbs, time_s, rate_inhr) 
#            VALUES (", fppr$porous_pavement_uid[i], ", ", fppr$weight_lbs[i], ", ", fppr$time_s[i], ", ", fppr$rate_inhr[i], ")"))
# }

#1.4 write to db. using DBI because it is in a schema and the odbc version doesn't really get that
library(DBI)

#dbWriteTable(mars, SQL("fieldwork.porous_pavement_results"), fppr, append = TRUE)


#table(fpp_raw_results$porous_pavement_uid)

#2.0 version of this with more data -------------------
#5/19/2021

#porous pavement results data
rejuve <- read_excel("C:/Users/nicholas.manna/Documents/R/working/porous/porous_rejuve_results.xlsx") %>% 
  mutate("test_location" = as.character(test_location))

rejuve_record_join <- rejuve %>% 
  left_join(fpp, by = c("smp_id", "date" = "test_date", "test_location"))

rrj_write <- rejuve_record_join %>% 
  dplyr::select("porous_pavement_uid", "rate_inhr")

dbWriteTable(mars, SQL("fieldwork.porous_pavement_results"), rrj_write, append = TRUE)

#adding more results data
more <- read_excel("C:/Users/nicholas.manna/Documents/R/working/porous/other_porous_results.xlsx")

more_record_join <- more %>% 
  left_join(fpp, by = c("smp_id", "date" = "test_date", "test_location"))

more_record_write <- more_record_join %>% 
  dplyr::select("porous_pavement_uid", "weight_lbs", "time_s", "rate_inhr")

dbWriteTable(mars, SQL("fieldwork.porous_pavement_results"), more_record_write, append = TRUE)
