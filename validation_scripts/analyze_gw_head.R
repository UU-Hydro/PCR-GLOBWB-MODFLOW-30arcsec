# packages needed and clear all available existing objects:
require('psych'); require('RColorBrewer'); require('timeSeries'); require('topmodel'); require('zoo');

most_consecutive_val = function(x, val = 1) {
  with(rle(x), max(lengths[values == val]))
}

# plotting function
plot_comparison <- function(outpltfl, model_measu, date, startDate, endDate, y_min, y_max, site, iqm, iqo, qre, rho, rmse, bias, yrev) {
  tss_dfr = model_measu
  #y_min         = -10
  #y_max         =  5

  outplott <- ggplot()
  outplott <- outplott +
    geom_line(data = tss_dfr[!is.na(tss_dfr$data),], aes(x = date, y = data), colour =  "green" , size = 0.35, linetype = "solid") +
    geom_point(data = tss_dfr, aes(x = date, y = data ), shape=1,  colour =  "green") +
    geom_line(data = tss_dfr, aes(x = date, y = model ), colour =  "red", size = 0.35) +
    scale_y_continuous(limits=c(y_min,y_max)) +
    scale_x_date('',limits=c(as.Date(startDate,"%Y-%m-%d"),as.Date(endDate,"%Y-%m-%d"))) +
    ggtitle(paste("site ",site, 
     "; iq_mod = ",sprintf(iqm,fmt='%#.2f'),
     ", iq_obs = ",sprintf(iqo,fmt='%#.2f'),
     ", qre = ",sprintf(qre,fmt='%#.2f'),
     ", rho = ",sprintf(rho,fmt='%#.2f'),
     ", rmse = ",sprintf(rmse,fmt='%#.2f'),
     ", bias = ",sprintf(bias,fmt='%#.2f'),sep=""))
  if (yrev){
   outplott <- outplott + scale_y_reverse()
   outplott <- outplott + ylab("Relative groundwater depth [m]")
  }else{
   outplott <- outplott + ylab("Relative groundwater head [m]")
  }
  outplott <- outplott + theme(axis.title.y = element_text(size = 10)) 
  outplott <- outplott + theme(plot.title = element_text(size = 10))
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
# percentile value that we used
  pairs      = length(pred[which(!is.na(obs))])
  percentius = c(0.25,0.75)
# calculate model and data percentile
  mdpr       = quantile(as.numeric(pred),probs=percentius,na.rm=T)
  mdpr7525   = as.numeric(mdpr[which(percentius==0.75)] - mdpr[which(percentius==0.25)])
  dtpr       = quantile(as.numeric(obs ),probs=percentius,na.rm=T)
  dtpr7525   = as.numeric(dtpr[which(percentius==0.75)] - dtpr[which(percentius==0.25)])
  QRE7525    = (mdpr7525-dtpr7525)/dtpr7525
  return(c(QRE7525,mdpr7525,dtpr7525))
#  return(QRE7525)
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
#plot_timeseries = TRUE

#region = "ADES"
#region = "DINO"
region = "USGS"
startDate = "1960-01-01"
endDate   = "2015-12-31"
#startDate = "2000-01-01"
#endDate   = "2002-12-31"
minYear   = 5
minAmp    = 0.0

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
 modelresults_folder = "f:/models/pcr-globwb-30arcsec/model_new/results_cartesius/transient/post_ts_usgs_txt_from_top/"
 station_with_xy_file = paste(modelresults_folder,"summary.txt",sep="")
}
output_folder = "f:/models/pcr-globwb-30arcsec/model_new/validation/statistics/"

########################################################################################################################################
########################################################################################################################################

output_folder = paste(output_folder,region,'/',sep="")
output_folder_plots = paste(output_folder,"plots",sep="")
dir.create(output_folder)
dir.create(output_folder_plots)
dir.create(paste(output_folder_plots,'/1/',sep=""))
dir.create(paste(output_folder_plots,'/2/',sep=""))
dir.create(paste(output_folder_plots,'/3/',sep=""))
dir.create(paste(output_folder_plots,'/4/',sep=""))

output_summary_txt = paste(output_folder,"summary.txt",sep="")

station_with_xy = read.table(station_with_xy_file, header =T) 

# list of the months that we want to use
monthly_used = unique(substr(as.character(seq(as.Date(startDate,"%Y-%m-%d"),as.Date(endDate,"%Y-%m-%d"),1)),1,7))

datseq = seq(as.Date(startDate,"%Y-%m-%d"),as.Date(endDate,"%Y-%m-%d"),1)
quarter_used = substr(quarters(as.Date(datseq)),2,2)
quarter_used = unique(paste(substr(monthly_used,1,4),quarter_used,sep="-"))

if (region == "ADES"){
 sites =  station_with_xy$EC		
}else if (region == "DINO"){
 sites = station_with_xy$stat_code
}else{
 sites = station_with_xy$site_no
}

first = TRUE
#for (is in 1:100) {
for (is in 1:length(sites)) {
 print(paste("***** Station ",is,"/",length(sites),"..."))

 #################################################################################################################################################################
 # read measurement file
 #################################################################################################################################################################

 namemsfile = paste(measurement_folder, sites[is],".txt", sep = "")		
 readmsfile = read.table(namemsfile,header=TRUE)		 		
 
 # throw away data without date
 readmsfile = readmsfile[(which(!is.na(readmsfile[,1]))),]		

 # throw away data without water depth or head
 if (region != "USGS"){
  wtd_flag = FALSE
  readmsfile = readmsfile[(which(!is.na(readmsfile$head))),]
 }else{
  n_wtd  = length(which(!is.na(readmsfile$lev_va)))
  n_head = length(which(!is.na(readmsfile$sl_lev_va)))
  if ((n_wtd == 0) & (n_head == 0)){
   print(paste("Skipping for:",sites[is],sep=" "))
   next
  }
  if (n_wtd > 0){
   use_wtd = TRUE
   if (n_head > n_wtd){
    use_wtd = FALSE
   }
  }else{
   use_wtd = FALSE
   if (n_wtd > n_head){
    use_wtd = TRUE
   }
  }

  #if (use_wtd){
  # print(paste("Skipping wtd for:",sites[is],sep=" "))
  # next
  #}

  if (use_wtd){
   wtd_flag = TRUE
   readmsfile = readmsfile[(which(!is.na(readmsfile$lev_va))),] # water-level feet below land surface
  }else{
   wtd_flag = FALSE
   readmsfile = readmsfile[(which(!is.na(readmsfile$sl_lev_va))),] # water-level feet above datum
  }
 }

 # using groundwater head data or depth
 if (region != "USGS"){
  readmsfile = cbind(as.character(readmsfile[,1]),as.numeric(readmsfile$head)) # use actual groundwater heads
 }else{
  if (wtd_flag){
   readmsfile = cbind(as.character(readmsfile[,4]),as.numeric(readmsfile$lev_va)*0.3048) # water table depth to meters
  }else{
   readmsfile = cbind(as.character(readmsfile[,4]),as.numeric(readmsfile$sl_lev_va)*0.3048) # water-level feet above datum
  }
 }
	 
 if (length(readmsfile) <= 2) {
  print(paste("Skipping for:",sites[is]," for not having enough data",sep=" "))
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
  readmsfile  =	  readmsfile[(which(as.numeric(as.Date(readmsfile[,1],"%Y-%m-%d")) > (as.numeric(as.Date(startDate,"%Y-%m-%d")) - 1))),]
 }
 if (length(readmsfile) > 2) {
  readmsfile  =	  readmsfile[(which(as.numeric(as.Date(readmsfile[,1],"%Y-%m-%d")) < (as.numeric(as.Date(  endDate,"%Y-%m-%d")) + 1))),]}
 }

 # converting to monthly time series
 if (length(readmsfile) > 2) {
  mongw_head  = mat.or.vec(length(monthly_used),1); mongw_head[] = NA
  quargw_head = mat.or.vec(length(quarter_used),1); quargw_head[] = NA
  for (im in 1:length(monthly_used)){
   mongw_head[im] = mean(as.numeric(readmsfile[which(substr(readmsfile[,1],1,7) == monthly_used[im]),2]),na.rm=T)
  }
  quarter_meas = paste(substr(readmsfile[,1],1,4),substr(quarters(as.Date(readmsfile[,1])),2,2),sep='-')
  for (iq in 1:length(quarter_used)){
   quargw_head[iq] = mean(as.numeric(readmsfile[which(quarter_meas == quarter_used[iq]),2]),na.rm=T)
  }
  quargw_head[!is.nan(quargw_head)] <- 1
  quargw_head[is.nan(quargw_head)] <- 0
  measured_data = data.frame(paste(as.character(monthly_used),"-15",sep=""),mongw_head)
  names(measured_data)[1] <- "date"
  names(measured_data)[2] <- "measurement"
  measured_data$date = as.Date(measured_data$date,"%Y-%m-%d")
 }

 # skip in case 
 ny = most_consecutive_val(quargw_head)/4
 if (ny < minYear){
  print(paste("Skipping for:",sites[is],', number of years found:',ny,sep=" "))
  next
 }  

 #################################################################################################################################################################
 # read model file
 #################################################################################################################################################################

 namemofile = paste(modelresults_folder, sites[is],".txt", sep = "")		
 readmofile = read.table(namemofile,header=TRUE)		 		
 
 # throw away unused data
 if (wtd_flag){
  readmofile = subset(readmofile,select=-head) # throw away head
 }else{
  readmofile = subset(readmofile,select=-wtd) # throw away wtd
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
 
 QRE7525_evalua_all = QRE7525(month_msdata,month_modelo); # print(paste("QRE7525_evalua",QRE7525_evalua,sep=" "))
 QRE7525_evalua  = QRE7525_evalua_all[1]
 IQ7525_moevalua = QRE7525_evalua_all[2]
 IQ7525_msevalua = QRE7525_evalua_all[3] 

 # classify
 if (abs(QRE7525_evalua) <= 0.5){
  if (RHO_p_month > 0.5){
   iperf = 1
  }else{
   iperf = 3
  }
 }else{
  if (RHO_p_month > 0.5){
   iperf = 2
  }else{
   iperf = 4
  }
 } 

 if (abs(IQ7525_msevalua) < minAmp){
  print(paste("Skipping for:",sites[is],', minmimum meas. amp found:',abs(IQ7525_msevalua),sep=" "))
  next
 }

 print(paste("IQ7525_msevalua:",IQ7525_msevalua,sep=" "))

 } # end if (length(readmsfile) > 2)

 #################################################################################################################################################################
 # plotting time series 
 #################################################################################################################################################################
 if ((length(readmsfile) > 2) & plot_timeseries) {
  ## Plotting the chart !!!
  model = data.frame(merged_date$date,month_modelo - avg_momonth); names(model) = cbind("date","model")
  measu = data.frame(merged_date$date,month_msdata - avg_msmonth); names(measu) = cbind("date", "data")
  if (wtd_flag){
   outpltfl = paste(output_folder_plots,"/",iperf,"/",sites[is],"_gwtd.png",sep="")
  }else{
   outpltfl = paste(output_folder_plots,"/",iperf,"/",sites[is],"_gwhead.png",sep="")
  }
  model_measu = merge(model,measu,by="date",all.x=TRUE)

  model_measu$date = as.Date(as.character(model_measu[,1]),"%Y-%m-%d")
  print(outpltfl)
  y_min = min(min(model_measu[,2],na.rm=T),min(model_measu[,3],na.rm=T))
  y_max = max(max(model_measu[,2],na.rm=T),max(model_measu[,3],na.rm=T))
  plot_comparison(outpltfl, model_measu, date, startDate, endDate, y_min, y_max, sites[is], 
   IQ7525_moevalua, IQ7525_msevalua, QRE7525_evalua, RHO_p_month, RMSE__month, BIAS__month, wtd_flag)
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
 if (first){
#  header = c("station_name",   "latitude",    "longitude",     "avg_msmonth",   "avg_momonth",   
#             "RHO_p_month" ,   "R2____month", "R2adj_month",   "NSeff_month",   "RMSE__month", 
#             "MAE___month",    "BIAS__month", "NSeff_month_nb","RMSE__month_nb","MAE___month_nb",
#             "BIAS__month_nb", "QRE7525_evalua")
  header = c("station_name",   "latitude",     "longitude",     "RHO_p_month",   "QRE7525_evalua")   
  cat(header,"\n",sep="\t",file=output_summary_txt,append=FALSE)
  first = FALSE
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
