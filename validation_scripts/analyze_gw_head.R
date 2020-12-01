# packages needed and clear all available existing objects:
require('psych'); require('RColorBrewer'); require('timeSeries'); require('topmodel'); require('zoo');

# plotting function
plot_comparison <- function(outpltfl, model_measu, date, y_min, y_max) {
  tss_dataframe = model_measu
  #y_min         = -10
  #y_max         =  5

  outplott <- ggplot()
  outplott <- outplott +
    geom_point(data = tss_dataframe,aes(x = date, y = data ), shape=4,  colour =  "green") +
    geom_line(data = tss_dataframe,aes(x = date, y = data ),   colour =  "green" , size = 0.90) +
    geom_line(data = tss_dataframe,aes(x = date, y = model ),  colour =  "red", size = 0.35) +
    scale_y_continuous(limits=c(y_min,y_max)) +
    scale_x_date('',limits=c(as.Date("1958-01-01","%Y-%m-%d"),as.Date("2015-12-31","%Y-%m-%d")))

  ggsave(paste(outpltfl,sep=""), plot = outplott,width=27,height=7,units='cm')

  rm(outplott)
}

# all functions:  ######################################################################################################################

rmse    <- function(obs, pred) sqrt(mean((obs-pred)^2 ,na.rm=T))
 mae    <- function(obs, pred)      mean(abs(obs-pred),na.rm=T)
bias    <- function(obs, pred) mean(pred[which(!is.na(obs) & !is.na(pred))]) - mean(obs[which(!is.na(obs) & !is.na(pred))])  # POSITIVE indicates that the average prediction is higher than average observation. 
R2      <- function(obs, pred) summary(lm(obs ~ pred))$r.squared
R2ad    <- function(obs, pred) summary(lm(obs ~ pred))$adj.r.squared

QRE7525 <- function(obs, pred) {
# pecentile value that we used
  pairs      = length(pred[which(!is.na(obs))])
  percentius = c(0.25,0.75)
# calculate model and data percentile
  mdpr       = quantile(as.numeric(pred),probs=percentius,na.rm=T)
  mdpr7525   = as.numeric(mdpr[which(percentius==0.75)] - mdpr[which(percentius==0.25)])
  dtpr       = quantile(as.numeric(obs ),probs=percentius,na.rm=T)
  dtpr7525   = as.numeric(dtpr[which(percentius==0.75)] - dtpr[which(percentius==0.25)])
  QRE7525    = (mdpr7525-dtpr7525)/dtpr7525
#~   return(c(QRE7525,dtpr7525,pairs))
  return(QRE7525)
}

NSeff <- function (Qobs, Qsim)
{
    Qsim <- Qsim[!is.na(Qobs)]
        Qobs <- Qobs[!is.na(Qobs)]
	    Qobs <- Qobs[!is.na(Qsim)]
	        Qsim <- Qsim[!is.na(Qsim)]
		    if (length(Qobs) == 0 || length(Qsim) == 0)
		            return(NA)
			        NS <- 1 - (sum((Qobs - Qsim)^2)/sum((Qobs - mean(Qobs))^2))
				    return(NS)
}

########################################################################################################################################
########################################################################################################################################

plot_timeseries = FALSE

#region = "ADES"
region = "DINO"
#region = "USGS"

# input files:
if (region == "ADES"){
 input_folder         = "f:/basis_data/Rhine-Meuse/gw_head_data_rhine-meuse-master/database/groundwater_head_rhine_meuse_basin/03_FRANCE_ADES/"
 station_with_xy_file = "./ADES/station_list_ades_sel.csv"
 measurement_folder   = paste(input_folder,"data_withdepth/",sep="")
 modelresults_folder  = "f:/models/pcr-globwb-30arcsec/model_new/results_cartesius/transient/post_ts_ADES_txt_from_top/"
}else if (region == "DINO"){
 input_folder         = "f:/basis_data/Rhine-Meuse/gw_head_data_rhine-meuse-master/database/groundwater_head_rhine_meuse_basin/11_DINO/"
 station_with_xy_file = "./DINO/dino_station_only_filtno1_sel_used.csv"
 measurement_folder   = paste(input_folder,"data_withdepth/",sep="")
 modelresults_folder  = "f:/models/pcr-globwb-30arcsec/model_new/results_cartesius/transient/post_ts_DINO_txt_from_top/"
}else{
 input_folder = "f:/models/pcr-globwb-30arcsec/model_new/validation/download_nwis/USGS/"
 measurement_folder = input_folder
 modelresults_folder = "f:/models/pcr-globwb-30arcsec/model_new/results_cartesius/transient/post_ts_USGS_txt_l2/"
 station_with_xy_file = paste(modelresults_folder,"summary.txt",sep="")
}
output_folder = "f:/models/pcr-globwb-30arcsec/model_new/validation/statistics/"

# period of interest
startDate = "1960-01-31"
endDate   = "2015-12-31"

########################################################################################################################################
########################################################################################################################################

output_folder = paste(output_folder,region,'/',sep="")
output_folder_plots = paste(output_folder,"plots",sep="")
dir.create(output_folder)
dir.create(output_folder_plots)

output_summary_txt = paste(output_folder,"summary.txt",sep="")

station_with_xy = read.table(station_with_xy_file, header =T) 

# list of the months that we want to use
monthly_used = unique(substr(as.character(seq(as.Date(startDate,"%Y-%m-%d"),as.Date(endDate,"%Y-%m-%d"),1)),1,7))

if (region == "ADES"){
 sites =  station_with_xy$EC		
}else if (region == "DINO"){
 sites = station_with_xy$stat_code
}else{
 sites = station_with_xy$site_no
}

#for (is in 1:1) {
for (is in 1:length(sites)) {

 #################################################################################################################################################################
 # read measurement file
 #################################################################################################################################################################

 namemsfile = paste(measurement_folder, sites[is],".txt", sep = "")		
 readmsfile = read.table(namemsfile,header=TRUE)		 		
 
 # throw away data without date
 readmsfile = readmsfile[(which(!is.na(readmsfile[,1]))),]		

 # throw away data without head
 if (region != "USGS"){
  readmsfile = readmsfile[(which(!is.na(readmsfile$head))),]
 }else{
  readmsfile = readmsfile[(which(!is.na(readmsfile$lev_va))),] # water-level feet below land surface
 }
 
 # using groundwater head data or depth
 if (region != "USGS"){
  readmsfile = cbind(as.character(readmsfile[,1]),as.numeric(readmsfile$head)) # use actual groundwater heads
 }else{
  readmsfile = cbind(as.character(readmsfile[,4]),as.numeric(readmsfile$lev_va)*0.3048) # water table depth to meters
 }
 
 if (length(readmsfile) <= 2){
  next
 }
	 
 # sort-ordering data based on date 
 readmsfile = readmsfile[order(as.Date(readmsfile[,1],"%Y-%m-%d")),]
 
 # all data for the entire period
 readmsfile_all = readmsfile[order(as.Date(readmsfile[,1],"%Y-%m-%d")),]

 # ignoring data outside the period of interest
 if (length(readmsfile) > 2) {
 # throwing NODATA VALUES (NA)
  readmsfile  =	  readmsfile[(which(!is.na(readmsfile[,1]))),]
  readmsfile  =	  readmsfile[(which(!is.na(readmsfile[,2]))),]
 if (length(readmsfile) > 2) {
  readmsfile  =	  readmsfile[(which(as.numeric(as.Date(readmsfile[,1],"%Y-%m-%d")) > (as.numeric(as.Date(startDate,"%Y-%m-%d")) - 1))),]}
 if (length(readmsfile) > 2) {
  readmsfile  =	  readmsfile[(which(as.numeric(as.Date(readmsfile[,1],"%Y-%m-%d")) < (as.numeric(as.Date(  endDate,"%Y-%m-%d")) + 1))),]}}

 # converting to monthly time series
 if (length(readmsfile) > 2) {
  mongw_head  =   mat.or.vec(length(monthly_used),1)	  ; mongw_head[] = NA
 for (im in 1:length(monthly_used))     		  {
  mongw_head[im] = mean(as.numeric(readmsfile[which(substr(readmsfile[,1],1,7) == monthly_used[im]),2]),na.rm=T)
  }
 measured_data = data.frame(paste(as.character(monthly_used),"-15",sep=""),mongw_head)
 names(measured_data)[1] <- "date"
 names(measured_data)[2] <- "measurement"
 measured_data$date = as.Date(measured_data$date,"%Y-%m-%d")
 }

 #################################################################################################################################################################
 # read model file
 #################################################################################################################################################################

 namemofile = paste(modelresults_folder, sites[is],".txt", sep = "")		
 readmofile = read.table(namemofile,header=TRUE)		 		
 
 # throw away data without head/wtd
 if (region != "USGS"){
  readmofile = subset(readmofile,select=-c(wtd) )
 }else{
  readmofile = subset(readmofile,select=-c(head) )
 }

 # converting to monthly time series
 mongw_head = mat.or.vec(length(monthly_used),1)
 mongw_head[] = NA
 for (im in 1:length(monthly_used)){
  mongw_head[im] = mean(as.numeric(readmofile[which(substr(readmofile[,1],1,7) == monthly_used[im]),2]),na.rm=T)
 }
 model_result = data.frame(paste(as.character(monthly_used),"-15",sep=""),mongw_head)
 names(model_result)[1] <- "date"
 names(model_result)[2] <- "model_result"
 model_result$date = as.Date(model_result$date,"%Y-%m-%d")

 #################################################################################################################################################################
 # merging model and measurement 
 #################################################################################################################################################################

 # test
 #model_result = measured_data
 #names(model_result)[2] <- "model_result"

 if (length(readmsfile) > 2) {
  merged_date = merge(measured_data, model_result, by = c("date"), all = TRUE)
 }
 
 #################################################################################################################################################################
 # evaluation
 #################################################################################################################################################################
 if (length(readmsfile) > 2) {
 month_msdata   = as.numeric(merged_date$measurement)
 month_modelo   = as.numeric(merged_date$model_result)

 # calculate monthly model performance
 avg_msmonth    =  mean(month_msdata,na.rm=T); # print(paste("avg_msmonth",avg_msmonth,sep=" "))
 avg_momonth    =  mean(month_modelo,na.rm=T); # print(paste("avg_momonth",avg_momonth,sep=" "))

 RHO_p_month    =   cor(month_msdata,month_modelo,use="complete.obs"); # print(paste("RHO_p_month",RHO_p_month,sep=" "))
 R2____month    =    R2(month_msdata,month_modelo); # print(paste("R2____month",R2____month,sep=" "))
 R2adj_month    =  R2ad(month_msdata,month_modelo); # print(paste("R2adj_month",R2adj_month,sep=" ")) 
 #
 NSeff_month    = NSeff(month_msdata,month_modelo); # print(paste("NSeff_month",NSeff_month,sep=" "))
 RMSE__month    =  rmse(month_msdata,month_modelo); # print(paste("RMSE__month",RMSE__month,sep=" "))
 MAE___month    =   mae(month_msdata,month_modelo); # print(paste("MAE___month",MAE___month,sep=" "))
 BIAS__month    =  bias(month_msdata,month_modelo); # print(paste("BIAS__month",BIAS__month,sep=" "))
 #
 NSeff_month_nb = NSeff(month_msdata-avg_msmonth,month_modelo-avg_momonth); # print(paste("NSeff_month_nb",NSeff_month_nb,sep=" "))
 RMSE__month_nb =  rmse(month_msdata-avg_msmonth,month_modelo-avg_momonth); # print(paste("RMSE__month_nb",RMSE__month_nb,sep=" "))
 MAE___month_nb =   mae(month_msdata-avg_msmonth,month_modelo-avg_momonth); # print(paste("MAE___month_nb",MAE___month_nb,sep=" "))
 BIAS__month_nb =  bias(month_msdata-avg_msmonth,month_modelo-avg_momonth); # print(paste("BIAS__month_nb",BIAS__month_nb,sep=" "))
 #
 QRE7525_evalua = QRE7525(month_msdata,month_modelo); # print(paste("QRE7525_evalua",QRE7525_evalua,sep=" "))
 } # end if (length(readmsfile) > 2)

 #################################################################################################################################################################
 # plotting time series 
 #################################################################################################################################################################
 if ((length(readmsfile) > 2) & plot_timeseries) {
  ## Plotting the chart !!!
  if (region != "USGS"){
   model = data.frame(merged_date$date,month_modelo - avg_momonth); names(model) = cbind("date","model")
   measu = data.frame(merged_date$date,month_msdata - avg_msmonth); names(measu) = cbind("date", "data")
   outpltfl = paste(output_folder_plots,"/",sites[is],"_gwhead.pdf",sep="")
  }else{
   model = data.frame(merged_date$date,month_modelo - avg_momonth); names(model) = cbind("date","model")
   measu = data.frame(merged_date$date,month_msdata - avg_msmonth); names(measu) = cbind("date", "data")
   outpltfl = paste(output_folder_plots,"/",sites[is],"_gwtd.pdf",sep="")
  }
  model_measu = merge(model,measu,by="date",all.x=TRUE)    			;

  model_measu$date = as.Date(as.character(model_measu[,1]),"%Y-%m-%d")
  print(outpltfl)
  y_min = min(min(model_measu[,2],na.rm=T),min(model_measu[,3],na.rm=T))
  y_max = max(max(model_measu[,2],na.rm=T),max(model_measu[,3],na.rm=T))
  plot_comparison(outpltfl, model_measu, date, y_min, y_max) 
 } # end if (length(readmsfile) > 2)

 #################################################################################################################################################################
 # summary
 #################################################################################################################################################################
 if (region == "ADES"){
  latitude  = as.character(station_with_xy$ylatdeg[is])
  longitude = as.character(station_with_xy$xlondeg[is])
 }else if (region == "DINO"){
  latitude  = as.character(station_with_xy$ylatdeg[is])
  longitude = as.character(station_with_xy$xlondeg[is])
 }else{
  latitude  = as.character(station_with_xy$dec_lat_va[is])
  longitude = as.character(station_with_xy$dec_long_va[is])
 }
 if (is == 1){
#  header = c("station_name",   "latitude",    "longitude",     "avg_msmonth",   "avg_momonth",   
#             "RHO_p_month" ,   "R2____month", "R2adj_month",   "NSeff_month",   "RMSE__month", 
#             "MAE___month",    "BIAS__month", "NSeff_month_nb","RMSE__month_nb","MAE___month_nb",
#             "BIAS__month_nb", "QRE7525_evalua")
  header = c("station_name",   "latitude",     "longitude",     "RHO_p_month",   "QRE7525_evalua")   
  cat(header,"\n",sep="\t",file=output_summary_txt,append=FALSE)
 }

 if (length(readmsfile) > 2) {
#  summary = c(sites[is],      latitude,    longitude,      avg_msmonth,    avg_momonth,   
#              RHO_p_month,    R2____month, R2adj_month,    NSeff_month,    RMSE__month,   
#              MAE___month,    BIAS__month, NSeff_month_nb, RMSE__month_nb, MAE___month_nb,
#              BIAS__month_nb, QRE7525_evalua)
  summary = c(sites[is],      latitude,     longitude,      RHO_p_month,     QRE7525_evalua)
  cat(summary,"\n",sep="\t",file=output_summary_txt,append=TRUE)
 } # end if (length(readmsfile) > 2)

} # end for (is in 1:length(station_with_xy$station))
print("***** Done *****")