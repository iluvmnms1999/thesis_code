setwd("/Users/emmawatts/github/packages/rosr/bf_sep")

# bf_sep script (modified) ------------------------------------------------
run=TRUE #ALLOWS SUCCESSIVE MODEL RUNS FOR DIFFERENT SITES

# DON'T REALLY WANT DEMO
# answer=readline(prompt='Do you want a demonstration of the model for the Merced River, USGS site 11266500? (yes or no) ')
# if(answer=='yes') {demo=TRUE} else {demo=FALSE}
# cat(fill=T)

# answer=readline(prompt=paste('Are the function, parameter, and input files in the working directory, ',getwd(),'? (yes or no) ', sep=''))
dir_fn=getwd()
dir_params=getwd()
dir_input=getwd()
dir_out=getwd()

# ANSWER SHOULDN'T BE NO BECAUSE FILES ARE IN THAT FOLDER
# if(answer=='no') {
#   cat('Full paths or paths from working directory must be specified.',fill=T)
#   cat('The default is the working directory (leave blank and press <Enter> key).',fill=T)
#   cat(fill=T)
#   answer=readline(prompt='Enter the directory with the workspace that has model functions (bf_sep.Rdata): ')
#   if(answer!='') {dir_fn=answer}
#   cat('Directory for workspace with functions: ', dir_fn,fill=T)
#   cat(fill=T)
#   answer=readline(prompt='Enter the directory with the parameter file (bf_params_usgs.csv): ')
#   if(answer!='') {dir_params=answer}
#   cat('Directory for parameter file: ',dir_params,fill=T)
#   cat(fill=T)
#   answer=readline(prompt='Enter the directory for output files: ')
#   if(answer!='') {dir_out=answer}
#   cat('Directory for output files: ',dir_out,fill=T)}
# cat(fill=T)

# LOAD WORKSPACE WITH FUNCTIONS
while(!('Rfunctions.bf_sep.Rdata' %in% dir(dir_fn))){cat('Workspace Rfunctions.bf_sep.Rdata is not available in ',dir_fn,'. Check where file is located.',fill=T)
  dir_fn=readline(prompt='Enter the directory that has Rfunctions.bf_sep.Rdata(use <Esc> key to stop the process): ')}
load(paste(dir_fn,'/Rfunctions.bf_sep.Rdata',sep=''))

while(!('bf_params_usgs.csv' %in% dir(dir_params))) {cat('Parameter file is not available in ',dir_params,'. Check where file is located.',fill=T)
  dir_params=readline(prompt='Enter the directory with bf_params_usgs.csv (use <Esc> key to stop the process)')}
params=read.csv(paste(dir_params,'/bf_params_usgs.csv',sep=''),colClasses=c('character',rep('numeric',17)))

#SPECIFICATIONS FOR DEMONSTRATION RUN
if(demo) {site_no='11266500';s=match(site_no,params$site_no);datestart='2010-10-01';dateend='2020-09-30'}

while(run) {#LOOP FOR SUCCESSIVE MODEL RUNS USING USER SPECIFICATION

  #USER SPECIFICATION FOR MODEL
  if(!demo) {s=NA

  #SPECIFY SITE
  site_no=readline(prompt='Enter USGS Site Number - include leading zeros:')
  s=match(site_no,params$site_no)

  while(is.na(s)) {tmp=params$site_no[substr(params$site_no,1,4) %in% substr(site_no,1,4)]
  cat('Parameters not available for site ',site_no)
  cat('Parameters are available for sites that match the first four characters:',tmp, fill=T)
  site_no=readline(prompt=('Enter USGS Site Number - include leading zeros (use <Esc> to stop): '))
  s=match(site_no,params$site_no)}

  #SPECIFY START AND END DATES
  answer=readline(prompt=paste('Start Date as 10-character string (YYYY-MM-DD, default is ',Sys.Date()-3652,': ',sep=''))
  if(as.Date(answer)>as.Date('1900-01-01')) {datestart=answer} else {datestart=Sys.Date()-3652}

  answer=readline(prompt=paste('End Date as 10-character string (YYYY-MM-DD, default is ',Sys.Date(),': ',sep=''))
  if(as.Date(answer)<Sys.Date()) {dateend=answer} else {dateend=Sys.Date()}
  cat(fill=TRUE)
  }

  cat('Site ', site_no, 'had a calibration error of', round(params$Error[s],3),fill=T)
  cat('Re-calibration may be appropriate for some applications','particularly when error is greater than ~ 0.06',fill=T)
  cat(fill=T)
  ########################################################################################################
  #STREAMFLOW DATA
  answer=readline(prompt='Retrieve streamflow data from NWIS using dataRetrieval package? (yes or no) ')
  #RETRIEVE FROM NWIS
  if(answer=='yes') {library(dataRetrieval)
    input_data=readNWISdv(site_no,parameterCd='00060',datestart,dateend,statCd='00003')
    dt=as.character(input_data[,3])
    qin=as.numeric(input_data[,4])
    unit_cd=1} else {

      #READ FROM LOCAL FILE
      cat(fill=T)
      cat('Daily streamflow values must be available as a comma-delimited (.csv), ASCII text file',fill=T)
      cat('with dates formatted as a 10-character string: YYYY-MM-DD.',fill=T)
      cat(fill=T)
      cat('Units may be cubic feet per second, cubic meters per second, or cubic meters per day ',fill=T)
      cat(fill=T)

      answer=readline(prompt='Enter the directory with streamflow input file(s), leave blank for working directory:')
      if(answer!='') {dir_input=answer}
      cat('Directory for input files: ',dir_input,fill=T)

      #SPECIFY STREAMFLOW FILE
      if(demo==TRUE) {qfile='streamflow.11266500.csv'; col_date=3;col_q=4;unit_cd=1} else {
        qfile=readline(prompt='Name of file with streamflow input_data (path to file will be added):')

        while(!(qfile %in% dir(dir_input))){cat('Cannot locate ',qfile,' in ',dir_input,'.',fill=T)
          qfile=readline(prompt=paste('Re-enter file name or <Esc> to stop: ',sep=''))}
        col_date=as.integer(readline(prompt='Enter the column number with dates: '))
        col_q=as.integer(readline(prompt='Enter the column number with streamflow: '))
        unit_cd=readline(prompt='Enter code for streamflow units (1 for CFS, 2 for M3/S, 3 for M3/DAY):')}

      #READ STREAFLOW FILE
      input_data=read.csv(paste(dir_input,'/', qfile,sep=''),stringsAsFactors=F)
      st=match(datestart,input_data[,col_date])
      en=match(dateend,input_data[,col_date])
      if(any(is.na(c(st,en)))){st=1; en=dim(input_data)[[1]]; datestart=input_data[1,col_date]; dateend=input_data[en,col_date]}
      cat(paste('Data available from ', datestart,' to ', dateend),fill=T)

      #ASSIGN VALUES TO ARGUMENTS FOR MAIN FUNCTION
      dt=input_data[,col_date] #VECTOR OF DATES
      qin=input_data[,col_q]} #VECTOR OF DAILY STREAMFLOW VALUES
  #########################################################################################################
  #CONVERT STREAMFLOW VALUE TO M3 PER DAY (NO CONVERSION IF unit_cd=3)
  if(unit_cd==1) {qin=qin/35.31*3600*24} #FOR CFS DATA
  if(unit_cd==2) {qin=qin*3600*24} #FOR CMS DATA

  #ADD 100-DAY PROJECTION
  dt=c(dt,as.character(as.Date(seq((as.Date(dateend)+1),(as.Date(dateend)+100),by='day'))))
  qin=c(qin,rep(NA,100))

  #ASSIGN VALUES FOR MODEL ARGUMENTS
  basin_char=c(params[s,2],params[s,3],params[s,4],params[s,5],params[s,6])
  gw_hyd=c(params[s,7],params[s,8],params[s,9],params[s,10],params[s,11])
  flow=c(params[s,12],params[s,13],params[s,14],params[s,15],params[s,16],params[s,17])
  timestep='day' #TIME STEP
  error_basis='total' #BASIS FOR CALCULATING MODEL ERROR

  #MAIN FUNCTION
  bf_sep(qin,dt,timestep,error_basis,basin_char,gw_hyd,flow)
  write.csv(bf_mod_out,paste('bf_mod_out.',site_no,'.csv',sep=''),row.names=F)
  cat('Model output is stored in the object bf_mod_out and has been saved to',paste('bf_mod_out.',site_no,'.csv',sep=''),fill=T)

  #PLOT OUTPUT
  hydgrph=paste('bf_sep_hydrograph.',site_no,'.pdf',sep='')
  pdf(hydgrph,height=6,width=9)
  plot(as.Date(dt),bf_mod_out$Qob.L3,xlab=NA,ylab='Streamflow [m3/day]',type='l',log='y')
  lines(as.Date(dt),bf_mod_out$Baseflow.L3,col='blue')
  dev.off()

  cat('Hydrographs can be viewed in the pdf file ',hydgrph,fill=T)

  answer=readline(prompt='Run model again? (yes or no) ')
  if(answer=='yes') {run=TRUE; demo=FALSE} else {run=FALSE}
} #CLOSES LOOP FOR SUCCESSIVE RUNS

# CHECK A FEW THINGS
setwd("/Users/emmawatts/github/packages/rosr")
usgs_huc <- readRDS("data-raw/usgs_fs/usgs_huc.RDS")
head(usgs_huc)
bf_params <- read.csv("bf_params_usgs.csv")
ind <- which(usgs_huc$site_no %in% bf_params$site_no)
usgs_available <- usgs_huc[ind]

# CONCLUSION
# baseflow is reported on a daily level rather than hourly and is also missing
# for a quarter of the stations we need data for so we're gonna have to do some
# estimating anyway
