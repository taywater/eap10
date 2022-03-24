#EAP table Query

mars_9 <- dbConnect(odbc(), "mars_testing")
mars_12 <- dbConnect(odbc(),"mars_data")

ow_data <- dbGetQuery(mars_9,
                      "SELECT gi.smp_smptype as smptype,
                              count(ow.ow_uid) as locations,
                              sum(ld.time_days) as deployment_length_days
                      FROM fieldwork.ow_all as ow
                      LEFT JOIN public.leveldata_time_per_ow as ld
                      ON ow.ow_uid = ld.ow_uid
                      LEFT JOIN public.greenit_smpbestdata as gi
                      ON gi.smp_id = ow.smp_id
                      GROUP BY gi.smp_smptype")



deployment_query <- dbGetQuery(mars_9,
                    "SELECT 	gi.smp_smptype,
                    		      count(dfc.deployment_uid) as locations,
                              sum(extract( day from (dfc.collection_dtime_est - dfc.deployment_dtime_est))) as monitor_length
                    FROM fieldwork.deployment_full_cwl as dfc
                    LEFT JOIN public.greenit_smpbestdata as gi
                    ON gi.smp_id = dfc.smp_id
                    WHERE collection_dtime_est IS NOT NULL
                    AND collection_dtime_est <= '20210101' AND collection_dtime_est >= '20130101'
                    AND public = TRUE
                    AND ow_suffix LIKE '%OW%'
                    AND smp_smptype IS NOT NULL
                    GROUP BY gi.smp_smptype")

#Query i
query_i <- dbGetQuery(mars_9,
                      "select count(*) from public.ow_leveldata_raw o 
                        left join fieldwork.ow_ownership oo on o.ow_uid = oo.ow_uid 
                        where oo.public = TRUE and
                        o.dtime_est >= '20130101' and
                        o.dtime_est <= '20210101'")

#Query ii
query_ii <- dbGetQuery(mars_9,
                      "with distinct_public_eap10 as 
                        (select distinct ow.smp_id from public.ow_leveldata_raw o 
                        left join fieldwork.ow_ownership oo on o.ow_uid = oo.ow_uid 
                        left join fieldwork.ow_all ow on o.ow_uid = ow.ow_uid
                        where oo.public = TRUE and
                        	o.dtime_est >= '20130101' and
                        	o.dtime_est <= '20210101')
                      
                        select count(*) from distinct_public_eap10")

#Query iii
querry_iii <- dbGetQuery(mars_9,
                               "SELECT 	gi.smp_smptype,
                    		      count(dfc.deployment_uid) as locations,
                              sum(extract( day from (dfc.collection_dtime_est - dfc.deployment_dtime_est))) as monitor_length
                    FROM fieldwork.deployment_full_cwl as dfc
                    LEFT JOIN public.greenit_smpbestdata as gi
                    ON gi.smp_id = dfc.smp_id
                    WHERE collection_dtime_est IS NOT NULL
                    AND collection_dtime_est <= '20210101' AND collection_dtime_est >= '20130101'
                    AND public = TRUE
                    AND ow_suffix LIKE '%OW%'
                    AND smp_smptype IS NOT NULL
                    GROUP BY gi.smp_smptype")

#Query iv
query_iv <- dbGetQuery(mars_9,
                       "select count(*) from public.baro 
                        where dtime_est >= '20130101' 
                        	and dtime_est <= '20210101'")
#Query v
query_v <- dbGetQuery(mars_9,
                        "select count(*) from public.rainfall_radarcell_raw where dtime_edt <= '20210101'")

#Query vi
query_vi <- dbGetQuery(mars_12,
                       "with all_public_events as (select rre.radar_event_uid, sr.smp_id, sr.radar_uid, df.collection_dtime_est, df.deployment_dtime_est, rre.eventdatastart_edt, rre.eventdataend_edt from data.radar_event rre 
                      	left join admin.smp_radar sr on rre.radar_uid = sr.radar_uid
                      	left join fieldwork.deployment_full df on sr.smp_id = df.smp_id
                      	left join fieldwork.ow_ownership oo on df.ow_uid = oo.ow_uid
                      	where oo.public = true),
                      	
                      	monitored_public_events_y10 as (select distinct ape.radar_event_uid, 
                      	ape.smp_id
                      	from all_public_events ape
                      	where ape.eventdatastart_edt >= ape.deployment_dtime_est and ape.eventdataend_edt <= ape.collection_dtime_est and ape.eventdataend_edt < '20210101')
                      
                        select count(*) from monitored_public_events_y10")
#Query vii
query_vii <- dbGetQuery(mars_9,
                       "with all_public_events as (select rre.rainfall_gage_event_uid, sr.smp_id, sr.gage_uid, df.collection_dtime_est, df.deployment_dtime_est, rre.eventdatastart_edt, rre.eventdataend_edt from rainfall_gage_event rre 
                      	left join smp_gage sr on rre.gage_uid = sr.gage_uid
                      	left join fieldwork.deployment_full df on sr.smp_id = df.smp_id
                      	left join fieldwork.ow_ownership oo on df.ow_uid = oo.ow_uid
                      	where oo.public = true),
                      	
                      	monitored_public_events_y10 as (select distinct ape.rainfall_gage_event_uid, 
                      	ape.smp_id
                      	from all_public_events ape
                      	where ape.eventdatastart_edt >= ape.deployment_dtime_est and ape.eventdataend_edt <= ape.collection_dtime_est and ape.eventdataend_edt < '20210101')
                      
                        select count(*) from monitored_public_events_y10")

#Query viii
query_viii <- dbGetQuery(mars_12,
                       "with simmed_events as (select distinct po. radar_event_uid, po.ow_uid, a.asset_type
                      	from metrics.overtopping  po 
                        		left join fieldwork.ow o
                         		on po.ow_uid = o.ow_uid
                      		left join external.assets a
                      			on o.smp_id = a.smp_id
                      		left join data.radar_event rre
                      			on po.radar_event_uid = rre. radar_event_uid
                      		left join admin.class_asset_surface cas
                      			on a.asset_type = cas.asset_type
                      	 where observed_simulated_lookup_uid = 2
                      		and a.component_id is null
                      		and a.asset_type is not null
                      		and cas.class_surface = 'Subsurface'
                      		and rre.eventdatastart_edt < '20210101'),
                      					   
                      simmed_results as (select distinct overtopping, po.radar_event_uid, observed_simulated_lookup_uid, po.ow_uid, se.asset_type
                      	from metrics.overtopping po 
                      	left join simmed_events se on po.radar_event_uid = se.radar_event_uid 
                      		and po.ow_uid = se.ow_uid
                      	where asset_type is not null)
                      
                      select asset_type as \"SMP Type\", 
                      sum((observed_simulated_lookup_uid = 2)::integer) as \"Total Simulated Events\", 
                      sum((overtopping = TRUE and observed_simulated_lookup_uid = 2)::integer) as \"Simulated Overtopping\", 
                      sum((overtopping = TRUE and observed_simulated_lookup_uid = 1)::integer) as \"Observed Overtopping\" 
                      from simmed_results group by asset_type")
#Query ix
query_ix <- dbGetQuery(mars_12,
                        "with pre_post_infil as (SELECT distinct ssla.smp_id, ssla.ow_suffix, round(ssla.mean_infiltration_rate_inhr, 4) as obs_rate_inhr, coalesce(gu.infil_dsg_rate_inhr, 0) as dsg_rate_inhr 
                          FROM metrics.subsurface_smp_level_aggregation ssla 
                          left join external.greenit_unified gu on ssla.smp_id = gu.smp_id 
                          where mean_infiltration_rate_inhr is not null order by smp_id),
                          
                          infil_exceedence as (select smp_id, obs_rate_inhr - dsg_rate_inhr as rate_increase_inhr, obs_rate_inhr > dsg_rate_inhr as underestimation from pre_post_infil)
                          
                          select count(*) as \"Monitored Infiltrating SMPs\", 
                          	sum(underestimation::integer) as \"SMPs with Observed > Pre-Con\",
                          	round((sum(underestimation::integer)::numeric / count(*)::numeric), 4) * 100 as \"Percentage of Exceeding Infiltrating SMPs\",
                          	round(avg(rate_increase_inhr), 4) as \"Average Increase over Pre-Con (in/hr)\" from infil_exceedence")



#Disconnect
dbDisconnect(mars_9)
dbDisconnect(mars_12)
