#Generate Infiltration x Infiltration comparison plot
library(odbc)
library(tidyverse)

mars <- dbConnect(odbc(), "mars_testing")

infils <- dbGetQuery(mars, "SELECT distinct ssla.smp_id, ssla.ow_suffix, ssla.mean_infiltration_rate_inhr as obs_rate_inhr, gu.infil_dsg_rate_inhr as dsg_rate_inhr FROM performance.subsurface_smp_level_aggregation ssla left join greenit_unified gu on ssla.smp_id = gu.smp_id where mean_infiltration_rate_inhr is not null order by smp_id")

#recode unrecorded preconstruction infiltration rates to 0
#it was policy for a time to simply not record precon infiltration rates if the test showed ~0 in/hr
infils$dsg_rate_inhr[is.na(infils$dsg_rate_inhr)] <- 0

#recode 0 to 0.02 for ease of plotting at log scale
infils$dsg_rate_inhr[infils$dsg_rate_inhr == 0] <- 0.01

#minor break lines
minor_breaks <- rep(1:9, 5)*(10^rep(-2:2, each=9))

ggplot(infils) + 
  geom_point(aes(x = dsg_rate_inhr, y = obs_rate_inhr)) + 
  geom_line(data = data.frame(x = c(0.01, 10), y = c(0.01, 10)), aes(x = x, y = y)) + 
  scale_x_continuous(trans = "log10") + 
  scale_y_continuous(trans = "log10", minor_breaks = minor_breaks) + 
  xlab("Log10 of Pre-Construction Infiltration Rate (in/hr)") + 
  ylab("Log10 of Observed Recession Rate (in/hr)") +  
  ggtitle("Pre-Construction Infiltration Rates vs Observed Rates") +
  theme(text = element_text(size = 20))

ggplot(infils) + 
  geom_point(aes(x = dsg_rate_inhr, y = obs_rate_inhr)) + 
  geom_line(data = data.frame(x = c(0.01, 10), y = c(0.01, 10)), aes(x = x, y = y)) + 
  scale_x_continuous(trans = "log10") + 
  scale_y_continuous(trans = "log10", minor_breaks = trans_breaks) + 
  xlab("Log10 of Pre-Construction Infiltration Rate (in/hr)") + 
  ylab("Log10 of Observed Recession Rate (in/hr)") +  
  ggtitle("Pre-Construction Infiltration Rates vs Observed Rates") +
  theme(text = element_text(size = 20))


