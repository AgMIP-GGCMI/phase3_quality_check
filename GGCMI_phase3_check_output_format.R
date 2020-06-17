require(ncdf4)

# Paths
landseamask_file <- "/project2/ggcmi/AgMIP.input/phase3/ISIMIP3/landseamask/landseamask_no_antarctica.nc"
report_dir <- "/home/chmueller/public_html/" # Set to "" to use working directory (top-level directory of outputs)
report_dir_web <- "https://users.rcc.uchicago.edu/~chmueller/" # Set to "" to ignore
ggcmi_function_file <- "./GGCMI_phase3_check_functions.r"

# get GGCM folder name passed as argument to script call
args <- commandArgs(trailingOnly = TRUE)
if (length(args)==0) {
  stop("GGCM folder name must be supplied (input file).", call.=FALSE)
}

# settings and definitions ####
crops <- c("mai","ri1","ri2","soy","swh","wwh","bar","rye","mil","sor","soy","sun","pot","cas","sgc","sgb","nut","cot","rap","bea","pea","mgr")
irrigs <- c("firr","noirr")
rcsps <- c("picontrol","historical","ssp126","ssp585")
socs <- c("2015soc")
sens <- c("default","2015co2")
gcms <- c("GFDL-ESM4","IPSL-CM6A-LR","MPI-ESM1-2-HR","MRI-ESM2-0","UKESM1-0-LL")
vars <- c("yield","biom","cnyield","plantday","plantyear","anthday","matyday","pirrww","aet","soilmoist1m",
          "transp","evap","runoff","rootm","tnrup","tnrin","tnrloss","n2oemis","n2emis","nleach","tcemis","ch4emis")
ranges <- list("yield"=c(0,100),"biom"=c(0,100),"cnyield"=c(0,100),"plantday"=c(0,365),"plantyear"=c(1850,2100),"anthday"=c(0,365),"matyday"=c(0,365),
              "pirrww"=c(0,1e6),"aet"=c(0,1e6),"soilmoist1m"=c(0,1e6),"transp"=c(0,1e6),
              "evap"=c(0,1e6),"runoff"=c(0,1e6),"rootm"=c(0,100),"tnrup"=c(0,1e4),"tnrin"=c(0,1e4),"tnrloss"=c(0,1e4),"n2oemis"=c(0,1e4),"n2emis"=c(0,1e4),
              "nleach"=c(0,1e4),"tcemis"=c(0,1e4),"ch4emis"=c(0,1e4))
units <- c("t ha-1 gs-1 (dry matter)","t ha-1 gs-1 (dry matter)","","day of year","calendar year","days from planting","days from planting","kg m-2 gs-1",
           "kg m-2 gs-1","kg m-3","kg m-2 gs-1","kg m-2 gs-1","t ha-1 gs-1 (dry matter)","kg ha-1 gs-1","kgN ha-1 gs-1","kgN ha-1 gs-1","kgN ha-1 gs-1",
           "gN m-2 gs-1","gN m-2 gs-1","gN m-2 gs-1","gC m-2 gs-1","gC m-2 gs-1")

# Source functions
source(ggcmi_function_file)

fname.issues <- list()
data.issues <- list()

landseamask <- readmask.nc(landseamask_file)

# Get and change working directory
working_dir <- paste0("/project2/ggcmi/AgMIP.output/",args[1],"/phase3b")
setwd(working_dir)

# If report_dir not specified, set it to working_dir
if report_dir == "" {
    report_dir = paste0(working_dir, "/")
}

# delete old reports
unlink(paste0(report_dir,args[1],"*"))
reportname <- paste0(report_dir,args[1],"_summary.txt")
fn.reportname <- paste0(report_dir,args[1],"_filename_issues.txt")
sim.reportname <- paste0(report_dir,args[1],"_simulations_missing.txt")
data.reportname <- paste0(report_dir,args[1],"_data_issues.txt")
if report_dir_web != "" {
    fn.reportname2 <- paste0(report_dir_web,args[1],"_filename_issues.txt")
    sim.reportname2 <- paste0(report_dir_web,args[1],"_simulations_missing.txt")
    data.reportname2 <- paste0(report_dir_web,args[1],"_data_issues.txt")
}

sink(file=reportname,append=F)
#outfile <- file(reportname,"wt")
files <- dir()
date <- date()
cat("********  GGCMI Phase 3 file check summary report ********\n\n")
cat(date,"\n\n")
cat("there are more detailed reports for specific aspects:\n")
if report_dir_web != "" {
    cat(fn.reportname2,"\n")
    cat(sim.reportname2,"\n")
    cat(data.reportname2,"\n")
} else {
    cat(fn.reportname,"\n")
    cat(sim.reportname,"\n")
    cat(data.reportname,"\n")
}

cat("/*=============================================================================================*/\n")
cat("/*===================      FILE NAMING ISSUES     =============================================*/\n")
cat("/*=============================================================================================*/\n")
warnings <- errors <- 0
error.types <- list("wrong file ending"=NULL,"inconsistent model/folder name"=NULL,"wrong GCM for climate"=NULL,
                    "unknown scenario"=NULL,"unknown soc setting"=NULL,"unknown sensitivty setting"=NULL,
                    "wrong variable"=NULL,"unknown crop"=NULL,"wrong irrigation setting"=NULL,
                    "wrong region"=NULL,"wrong time step"=NULL,"wrong start year"=NULL,"wrong end year"=NULL,"wrong bias adjustment"=NULL)
for(fn in 1:length(files)){
  test <- test.filename(files[fn])
  warnings <- warnings + test$warnings
  errors <- errors + test$errors
  if(!is.null(test$ending.f)) error.types[[1]] <- c(error.types[[1]],fn)
  if(!is.null(test$mname.f)) error.types[[2]] <- c(error.types[[2]],fn)
  if(!is.null(test$climate.f)) error.types[[3]] <- c(error.types[[3]],fn)
  if(!is.null(test$scen.f)) error.types[[4]] <- c(error.types[[4]],fn)
  if(!is.null(test$soc.f)) error.types[[5]] <- c(error.types[[5]],fn)
  if(!is.null(test$sens.f)) error.types[[6]] <- c(error.types[[6]],fn)
  if(!is.null(test$var.f)) error.types[[7]] <- c(error.types[[7]],fn)
  if(!is.null(test$crop.f)) error.types[[8]] <- c(error.types[[8]],fn)
  if(!is.null(test$irrig.f)) error.types[[9]] <- c(error.types[[9]],fn)
  if(!is.null(test$region.f)) error.types[[10]] <- c(error.types[[10]],fn)
  if(!is.null(test$timestep.f)) error.types[[11]] <- c(error.types[[11]],fn)
  if(!is.null(test$starty.f)) error.types[[12]] <- c(error.types[[12]],fn)
  if(!is.null(test$endy.f)) error.types[[13]] <- c(error.types[[13]],fn)
  if(!is.null(test$bias.f)) error.types[[14]] <- c(error.types[[14]],fn)
  collected <- paste0(if(!is.null(test$ending.f))test$ending.f,if(!is.null(test$mname.f))test$mname.f,
                                                if(!is.null(test$climate.f))test$climate.f,if(!is.null(test$bias.f))test$bias.f,
                                                if(!is.null(test$scen.f))test$scen.f,if(!is.null(test$soc.f))test$soc.f,
                                                if(!is.null(test$sens.f))test$sens.f,if(!is.null(test$var.f))test$var.f,
                                                if(!is.null(test$crop.f))test$crop.f,if(!is.null(test$irrig.f))test$irrig.f,
                                                if(!is.null(test$region.f))test$region.f,if(!is.null(test$timestep.f))test$timestep.f,
                                                if(!is.null(test$starty.f))test$starty.f,if(!is.null(test$endy.f))test$endy.f)
  if(length(collected)>0)
    fname.issues[length(fname.issues)+1] <- paste0("file naming issues (",test$warnings," warnings; ",test$errors," errors) with ",files[fn],"\n",collected)
}
if(length(fname.issues)>0){
  cat(length(fname.issues),"file names issues in ",length(files)," files, with ",warnings,"Warnings and ",errors,"errors.\n\n")
  #indent.switch(indent=4)
  cat(fname.issues[[1]],sep="\n")  
  if(length(fname.issues)>2)
    cat("skipping",length(fname.issues)-2,"other examples...\n")
  if(length(fname.issues)>1)
    cat(fname.issues[[length(fname.issues)]],sep="\n")
  cat("\n ERROR types\n")
  counter <- 1
  for(i in 1:length(error.types)){
    if(length(error.types[[i]])>0){
      cat(counter,": ",names(error.types)[i],": in ",length(error.types[[i]])," files, e.g. ",files[error.types[[i]][1]],"\n",sep="")
      counter <- counter +1 
    }
  }
}

# stop reporting
sink()

sink(file=fn.reportname,append=F)
cat("********  GGCMI Phase 3 file check report ********\n\n")
cat(date,"\n\n")
if(length(fname.issues)>0){
  cat(unlist(fname.issues),sep="\n")
} else {
  cat("no file naming issues detected.\n")
}
sink()

# store model name for later
bits <- unlist(strsplit(files[1],"[.]"))
bits <- unlist(strsplit(bits[1],"_"))
model.name <- bits[1]




# testing completeness #####

# testing as all file names are wrong
sens <- c(sens,"transco2")


sims <- array(NA,dim=c(length(crops),length(irrigs),length(rcsps),length(socs),length(sens),length(gcms),length(vars)))
dimnames(sims) <- list(crops,irrigs,rcsps,socs,sens,gcms,vars)
#<modelname>_<climate_forcing>_<bias_adjustment>_<climate_scenario>_<soc_scenario>_<sens_scenario>_<variable>-<crop>-<irrigation>_<region>_<timestep>_<start_year>_<end_year>.nc

for(crop in 1:length(crops)){
  for(irrig in 1:length(irrigs)){
    for(rcsp in 1:length(rcsps)){
      for(soc in 1:length(socs)){
        for(sen in 1:length(sens)){
          for(gcm in 1:length(gcms)){
            for(var in 1:length(vars)){
              fn <- paste0(model.name,"_",tolower(gcms[gcm]),"_w5e5_",rcsps[rcsp],"_",socs[soc],"_",sens[sen],"_",vars[var],"-",crops[crop],"-",irrigs[irrig],"_global_annual_",ifelse(rcsp<3,1850,2015),"_",ifelse(rcsp==2,2014,2100),".nc")
              #fn <- paste0(model.name,"_",tolower(gcms[gcm]),"_w5e5_",rcsps[rcsp],"_",socs[soc],"_transco2_",vars[var],"-",crops[crop],"-",irrigs[irrig],"_global_annual_",ifelse(rcsp<3,1850,2015),"_",ifelse(rcsp==2,2014,2100),".nc")
              if(file.exists(fn)){
                sims[crop,irrig,rcsp,soc,sen,gcm,var] <- 1
              }
            }
          }
        }
      }
    }
  }
}

sink(file=reportname,append=T)
cat("\n\n\n/*=============================================================================================*/\n")
cat("/*===================      MISSING OUTPUTS        =============================================*/\n")
cat("/*=============================================================================================*/\n")


if(!all(!is.na(sims))){
  mcrops <- mirrigs <- mrcsps <- msocs <- msens <- mgcms <- mvars <- NULL
  for(crop in 1:length(crops)){
    if(all(is.na(sims[crop,,,,,,]))){
      mcrops <- c(mcrops,crop)
    }
  }
  if(length(mcrops)>0){
    cat("missing crops (",length(mcrops),"of",length(crops),"):",paste(crops[mcrops],collapse=", "),"\n")
  }
  for(irrig in 1:length(irrigs)){
    if(all(is.na(sims[,irrig,,,,,]))){
      mirrigs <- c(mirrigs,irrig)
    }
  }
  if(length(mirrigs)>0){
    cat("missing irrigation settings (",length(mirrigs),"of",length(irrigs),"):",paste(irrigs[mirrigs],collapse=", "),"\n")
  }
  for(rcsp in 1:length(rcsps)){
    if(all(is.na(sims[,,rcsp,,,,]))){
      mrcsps <- c(mrcsps,rcsp)
    }
  }
  if(length(mrcsps)>0){
    cat("missing scenarios (",length(mrcsps),"of",length(rcsps),"):",paste(rcsps[mrcsps],collapse=", "),"\n")
  }
  for(soc in 1:length(socs)){
    if(all(is.na(sims[,,,soc,,,]))){
      msocs <- c(msocs,soc)
    }
  }
  if(length(msocs)>0){
    cat("missing soc settings (",length(msocs),"of",length(socs),"):",paste(socs[msocs],collapse=", "),"\n")
  }
  for(sen in 1:length(sens)){
    if(all(is.na(sims[,,,,sen,,]))){
      msens <- c(msens,sen)
    }
  }
  if(length(msens)>0){
    cat("missing sensitivity scenarios (",length(msens),"of",length(sens),"):",paste(sens[msens],collapse=", "),"\n")
  }
  for(gcm in 1:length(gcms)){
    if(all(is.na(sims[,,,,,gcm,]))){
      mgcms <- c(mgcms,gcm)
    }
  }
  if(length(mgcms)>0){
    cat("missing gcms (",length(mgcms),"of",length(gcms),"):",paste(gcms[mgcms],collapse=", "),"\n")
  }
  for(var in 1:length(vars)){
    if(all(is.na(sims[,,,,,,var]))){
      mvars <- c(mvars,var)
    }
  }
  if(length(mvars)>0){
    cat("missing variables (",length(mvars),"of",length(vars),"):",paste(vars[mvars],collapse=", "),"\n")
  }
  
  
  # partially missing things
  sims2 <- sims[if(!is.null(mcrops))c(1:length(crops))[-mcrops] else 1,if(!is.null(mirrigs))c(1:length(irrigs))[-mirrigs] else 1,
                if(!is.null(mrcsps))c(1:length(rcsps))[-mrcsps] else 1,if(!is.null(msocs))c(1:length(socs))[-msocs] else 1,
                if(!is.null(msens))c(1:length(sens))[-msens] else 1,if(!is.null(mgcms))c(1:length(gcms))[-mgcms] else 1,
                if(!is.null(mvars))c(1:length(vars))[-mvars] else 1]
  
  if(!all(sims2==1)){
    cat("incomplete sets:",length(sims2[sims2!=1]),"of",length(sims2),"see",sim.reportname,"for details.\n")
    sink()
    sink(file=sim.reportname,append=F)
    cat("incomplete sets:\n")
    sims2[sims2!=1] <- "miss"
    sims2[sims2==1] <- "OK"
    print(sims2)
    sink()
    
  }
  #c(1:length(crops))[-mcrops]
} else {
  cat("\nno issues detected\n\n")
}
#sink()

  
cat("\n\n\n/*=============================================================================================*/\n")
cat("/*===================      DATA RANGE and COVERAGE ISSUES      ================================*/\n")
cat("/*=============================================================================================*/\n")
warnings <- errors <- 0
error.types <- list("variable isssues"=NULL,"number of dimensions"=NULL,"dimension names"=NULL,
                    "dimension definitions"=NULL,"units"=NULL,"data ranges"=NULL,
                    "data coverage"=NULL)
for(fn in 1:length(files)){
  test <- test.file(files[fn])
  warnings <- warnings + test$warnings
  errors <- errors + test$errors
  if(!is.null(test$var.f)) error.types[[1]] <- c(error.types[[1]],fn)
  if(!is.null(test$ndim.f)) error.types[[2]] <- c(error.types[[2]],fn)
  if(!is.null(test$dimname.f)) error.types[[3]] <- c(error.types[[3]],fn)
  if(!is.null(test$units.f)) error.types[[4]] <- c(error.types[[4]],fn)
  if(!is.null(test$range.f)) error.types[[5]] <- c(error.types[[5]],fn)
  if(!is.null(test$cover.f)) error.types[[6]] <- c(error.types[[6]],fn)
  
  collected <- paste0(if(!is.null(test$var.f))test$var.f,if(!is.null(test$ndim.f))test$ndim.f,
                      if(!is.null(test$dimname.f))test$dimname.f,if(!is.null(test$units.f))test$units.f,
                      if(!is.null(test$range.f))test$range.f,if(!is.null(test$cover.f))test$cover.f)
  if(length(collected)>0)
    data.issues[length(data.issues)+1] <- paste0("data range and coverage issues (",test$warnings," warngings; ",test$errors," errors) with ",files[fn],"\n",collected)
}
if(length(data.issues)>0){
  cat(length(data.issues),"file names issues in ",length(files)," files, with ",warnings,"Warnings and ",errors,"errors.\n\n")
  #indent.switch(indent=4)
  cat(data.issues[[1]],sep="\n")  
  if(length(data.issues)>2)
    cat("skipping",length(data.issues)-2,"other examples...\n")
  if(length(data.issues)>1)
    cat(data.issues[[length(data.issues)]],sep="\n")
  cat("\n ERROR types\n")
  counter <- 1
  for(i in 1:length(error.types)){
    if(length(error.types[[i]])>0){
      cat(counter,": ",names(error.types)[i],": in ",length(error.types[[i]])," files, e.g. ",files[error.types[[i]][1]],"\n",sep="")
      counter <- counter +1 
    }
  }
}

# stop reporting
sink()

sink(file=data.reportname,append=F)
cat("********  GGCMI Phase 3 data range and coverage check report ********\n\n")
cat(date,"\n\n")
if(length(data.issues)>0){
  cat(unlist(data.issues),sep="\n")
} else {
  cat("no data range and coverage issues detected.\n")
}
sink()
