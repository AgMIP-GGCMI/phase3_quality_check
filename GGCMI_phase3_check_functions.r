ranges <- list("yield"=c(0,100),"biom"=c(0,100),"cnyield"=c(0,100),"plantday"=c(0,365),"plantyear"=c(1850,2100),"anthday"=c(0,365),"matyday"=c(0,365),
              "pirrww"=c(0,1e6),"aet"=c(0,1e6),"soilmoist1m"=c(0,1e6),"transp"=c(0,1e6),
              "evap"=c(0,1e6),"runoff"=c(0,1e6),"rootm"=c(0,100),"tnrup"=c(0,1e4),"tnrin"=c(0,1e4),"tnrloss"=c(0,1e4),"n2oemis"=c(0,1e4),"n2emis"=c(0,1e4),
              "nleach"=c(0,1e4),"tcemis"=c(0,1e4),"ch4emis"=c(0,1e4))
              
readmask.nc <- function(filename,lo="lon",la="lat"){
  nc <- nc_open(filename)
  var <- names(nc$var)[1]
  lon <- ncvar_get(nc,lo)
  if(min(lon)>=0){
    cat("WARNING! Longitude does not contain negative values, shifting >180 by 360\n")
    lon[lon>180] <- lon[lon>180]-360
  }
  lat <- ncvar_get(nc,la)
  mask <- ncvar_get(nc,var)
  nc_close(nc)
  list(mask=mask,lon=lon,lat=lat)
}

target_units <- function(var_in) {
  if (var_in == "plantyear") {
    units <- "calendar year"
  } else if (var_in == "plantday") {
    units <- "day of year"
  } else if (is.element(var_in, c("anthday", "matyday"))) {
    units <- "days from planting"
  } else if (is.element(var_in, c("tcemis", "ch4emis", "n2oemis", "n2emis", "nleach"))) {
    units <- "gC m-2 gs-1"
  } else if (var_in == "rootm") {
    units <- "kg ha-1 gs-1"
  } else if (is.element(var_in, c("pirrww", "aet", "transp", "evap"))) {
    units <- "kg m-2 gs-1"
  } else if (var_in == "soilmoist1m") {
    units <- "kg m-3"
  } else if (is.element(var_in, c("tnrup", "tnrin", "tnrloss"))) {
    units <- "kgN ha-1 gs-1"
  } else if (is.element(var_in, c("yield", "biom", "runoff"))) {
    units <- "t ha-1 gs-1 (dry matter)"
  } else if (var_in == "cnyield") {
      units <- ""
  } else {
    stop(paste(var_in, "not recognized when trying to get units"))
  }
    
  return(units)
}

test.filename <- function(file_path, model.name){
  # <modelname>_<climate_forcing>_<bias_adjustment>_<climate_scenario>_<soc_scenario>_<sens_scenario>_<variable>-<crop>-<irrigation>_<region>_<timestep>_<start_year>_<end_year>.nc
  ending.f <- mname.f <- climate.f <- bias.f <- scen.f <- soc.f <- sens.f <- var.f <- crop.f <- irrig.f <- region.f <- timestep.f <- 
    starty.f <- endy.f <- NULL
  warnings <- errors <- 0
  
  # Get filename
  fn <- basename(file_path)
  
  # split up file name into elements
  # remove .nc parg
  bits <- unlist(strsplit(fn,"[.]"))
  ending <- bits[2]
  if(ending != "nc"){
    ending.f <- paste("  => WARNING: File ending is not '.nc' but",ending,"\n")
    warnings <- warnings + 1
  }
  # split rest of file name string into elements
  bits <- unlist(strsplit(bits[1],"_"))
  if(bits[1]!=tolower(model.name)){
    if (tolower(bits[1])==tolower(model.name)) {
      mname.f <- "  => WARNING: <modelname> in filename should be all lowercase\n"
    } else {
      mname.f <- paste0("  => WARNING: <modelname> in filename (",bits[1],") does not match value provided in script (",model.name,")\n")
    }
    warnings <- warnings + 1
    browser()
  }
  if(!(bits[2]%in%tolower(gcms))){
    browser()
    climate.f <- paste("  => ERROR: climate",bits[2],"not in set of GCMs\n")
    errors <- errors + 1
  }
  if(bits[3]!="w5e5"){
    browser()
    bias.f <- paste("  => ERROR: bias_adjustment string",bits[3],"not 'w5e5'\n")
    errors <- errors + 1 
  }
  if(!(bits[4]%in%rcsps)){
    browser()
    soc.f <- paste("  => ERROR: SSP/RCP scenario",bits[4],"not in set of scenarios",rcsps,"\n")
    errors <- errors + 1
  }
  if(!(bits[5]%in%tolower(socs))){
    browser()
    soc.f <- paste("  => ERROR: soc scenario",bits[5],"not in set of soc scenarios\n")
    errors <- errors + 1
  }
  if(!(bits[6]%in%tolower(sens))){
    browser()
    sens.f <- paste("  => ERROR: sens scenario",bits[6],"not in set of soc scenarios\n")
    errors <- errors + 1
  }
  bits2 <- unlist(strsplit(bits[7],"-"))
  if(length(bits2)!=3){
    browser()
    var.f <- paste("  => ERROR: wrong number of elements in variable string:",bits[7],"\n")
    errors <- errors + 1
  }
  else{
    if(!(bits2[1]%in%vars)){
      browser()
      var.f <- paste("  => ERROR: variable",bits2[1],"not in set of variables\n")
      errors <- errors + 1
    }
    if(!(bits2[2]%in%crops)){
      browser()
      crops.f <- paste("  => WARNING: variable",bits2[2],"not in set of crops\n")
      warnings <- warnings + 1
    }
    if(!(bits2[3]%in%irrigs)){
      browser()
      irrig.f <- paste("  => WARNING: variable",bits2[2],"not in set of crops\n")
      warnings <- warnings + 1
    }
  }
  if(!(bits[8]=="global")){
    browser()
    region.f <- paste("  => ERROR: region",bits[8],"not 'global'\n")
    errors <- errors + 1
  }
  if(!(bits[9]=="annual")){
    browser()
    timestep.f <- paste("  => ERROR: timestep",bits[9],"not 'annual'\n")
    errors <- errors + 1
  }
  if(!((bits[4]%in%rcsps[c(1,2)] & bits[10]==1850) | (bits[4]%in%rcsps[-c(1,2)] & bits[10]==2015))){
    browser()
    starty.f <- paste("  => ERROR: startyear",bits[10],"not compatible with scenario",bits[4],"\n")
    errors <- errors + 1
  }
  if(!((bits[4]==rcsps[2] & bits[11]==2014) | (bits[4]%in%rcsps[-2] & bits[11]==2100))){
    browser()
    endy.f <- paste("  => ERROR: endyear",bits[11],"not compatible with scenario",bits[4],"\n")
    errors <- errors + 1
  }
  list(ending.f=ending.f,mname.f=mname.f,climate.f=climate.f,bias.f=bias.f,scen.f=scen.f,soc.f=soc.f,sens.f=sens.f,
       var.f=var.f,crop.f=crop.f,irrig.f=irrig.f,region.f=region.f,timestep.f=timestep.f,starty.f=starty.f,endy.f=endy.f,
       warnings=warnings,errors=errors)
}

test.file <- function(fn, landseamask){
  var.f <- ndim.f <- dimname.f <- dimdef.f <- units.f <- range.f <- cover.f <- NULL  
  warnings <- errors <- 0

  bits <- unlist(strsplit(fn,"[.]"))
  # split first part of file name string into elements
  bits <- unlist(strsplit(bits[1],"_"))
  bits2 <- unlist(strsplit(bits[7],"-"))
  index <- which(vars==bits2[1])
  if(length(index)>0){
    nc <- nc_open(fn)
    if(nc$ndims!=3){
      ndim.f <- paste("  => ERROR: wrong number of dimensions",nc$ndims,"\n")
      errors <- errors + 1
    }
    var <- names(nc$var)[1]
    if(var!=paste0(bits2[1],"_",bits2[2])){
      var.f <- paste("  => ERROR: variable incorrectly named",var,"instead of",paste0(bits2[1],"_",bits2[2],".\n"))
      errors <- errors + 1
    }
    if(nc$var[[1]]$units!=target_units(var)){
      units.f <- paste(units.f,"  => ERROR: variable units incorrectly defined",nc$var[[1]]$units,"instead of '",target_units(var),"'\n")
      errors <- errors + 1
    }
    if(nc$var[[1]]$missval!=1e20){
      range.f <- paste(range.f,"  => ERROR: variable missval incorrectly defined",nc$var[[1]]$missval,"instead of '1e20'\n")
      errors <- errors + 1
    }
    
    data <- ncvar_get(nc,var)
    test1 <- data[,,1]
    test2 <- data[,,dim(data)[3]]
    if(!all(is.finite(test1[landseamask$mask==1]))){
      cover.f <- paste(cover.f,"  => WARNING: not all land with valid values in first time step (",length(which(is.finite(test1[landseamask$mask==1]))),"of",length(which(landseamask$mask==1)),")\n")
      warnings <- warnings + 1
    }
    if(!all(!is.finite(test1[landseamask$mask==0]))){
      cover.f <- paste(cover.f,"  => ERROR: not all ocean with invalid values in first time step (",length(which(!is.finite(test1[landseamask$mask==0]))),"of",length(which(landseamask$mask==0)),")\n")
      errors <- errors + 1
    }
    if(!all(is.finite(test2[landseamask$mask==1]))){
      cover.f <- paste(cover.f,"  => WARNING: not all land with valid values in last time step (",length(which(is.finite(test2[landseamask$mask==1]))),"of",length(which(landseamask$mask==1)),")\n")
      warnings <- warnings + 1
    }
    if(!all(!is.finite(test2[landseamask$mask==0]))){
      cover.f <- paste(cover.f,"  => ERROR: not all ocean with invalid values in last time step (",length(which(!is.finite(test2[landseamask$mask==0]))),"of",length(which(landseamask$mask==0)),")\n")
      errors <- errors + 1
    }
    data.r <- range(data,na.rm=T)
    if(min(data.r) < ranges[[index]][1] | max(data.r) > ranges[[index]][2]){
      range.f <- paste(range.f,"  => WARNING: data points (range:",paste(data.r,collapse=" "),") outside valid range",paste(ranges[[index]],collapse=" "),"\n")
      warnings <- warnings + 1
    }
    if(!("lat" %in% names(nc$dim))){
      dimname.f <- paste(dimname.f,"  => ERROR: latitude dimension 'lat' missing")
      errors <- errors + 1
    } else {
      lat <- ncvar_get(nc,"lat")
      if(!all(lat==landseamask$lat)){
        dimdef.f <- paste(dimdef.f,"  => ERROR: latitude dimension is incorrectly defined, ranging from",lat[1],"to",lat[length(lat)],"by",lat[2]-lat[1],"\n")
        errors <- errors + 1
      }
      if(nc$dim$lat$units!="degrees_north"){
        dimdef.f <- paste(dimdef.f,"  => ERROR: latitude units incorrectly defined",nc$dim$lat$units,"instead of 'degrees_north'\n")
        errors <- errors + 1
      }
    }
    if(!("lon" %in% names(nc$dim))){
      dimname.f <- paste(dimname.f,"  => ERROR: longitude dimension 'lon' missing")
      errors <- errors + 1
    } else {
      lon <- ncvar_get(nc,"lon")
      if(!all(lon==landseamask$lon)){
        dimdef.f <- paste(dimdef.f,"  => ERROR: longitude dimension is incorrectly defined, ranging from",lon[1],"to",lon[length(lon)],"by",lon[2]-lon[1],"\n")
        errors <- errors + 1
      }
      if(nc$dim$lon$units!="degrees_east"){
        dimdef.f <- paste(dimdef.f,"  => ERROR: longitude units incorrectly defined",nc$dim$lon$units,"instead of 'degrees_east'\n")
        errors <- errors + 1
      }
    }
    if(!("time" %in% names(nc$dim))){
      dimname.f <- paste(dimname.f,"  => ERROR: time dimension 'time' missing")
      errors <- errors + 1
    } else {
      time <- ncvar_get(nc,"time")
      if(!all(lat==landseamask$lat)){
        dimdef.f <- paste(dimdef.f,"  => ERROR: latitude dimension is incorrectly defined, ranging from",lat[1],"to",lat[length(lat)],"by",lat[2]-lat[1],"\n")
        errors <- errors + 1
      }
      if(nc$dim$time$units!="growing seasons since 1850-01-01 00:00:00"){
        dimdef.f <- paste(dimdef.f,"  => ERROR: time units incorrectly defined",nc$dim$time$units,"instead of 'growing seasons since 1850-01-01 00:00:00'\n")
        errors <- errors + 1
      }
    }
    
  } else {
    var.f <- paste("  => ERROR: Variable",bits2[1],"unknown\n")
    errors <- errors + 1
  }
  list(var.f=var.f,ndim.f=ndim.f,dimname.f=dimname.f,dimdef.f=dimdef.f,units.f=units.f,range.f=range.f,cover.f=cover.f,warnings=warnings,errors=errors)
}


setup_reports <- function(report_dir, report_dir_web, save2file, thisdate, model.name) {
  
  # If report_dir not specified, set it to working_dir
  if (report_dir == "") {
    report_dir = paste0(working_dir, "/")
  }
  
  # delete old reports
  unlink(paste0(report_dir,model.name,"*"))
  reportname <- paste0(report_dir,model.name,"_summary.txt")
  fn.reportname <- paste0(report_dir,model.name,"_filename_issues.txt")
  sim.reportname <- paste0(report_dir,model.name,"_simulations_missing.txt")
  data.reportname <- paste0(report_dir,model.name,"_data_issues.txt")
  reportnames <- list("fn"=fn.reportname, "sim"=sim.reportname, "data"=data.reportname)
  if (report_dir_web != "") {
    fn.reportname2 <- paste0(report_dir_web,model.name,"_filename_issues.txt")
    sim.reportname2 <- paste0(report_dir_web,model.name,"_simulations_missing.txt")
    data.reportname2 <- paste0(report_dir_web,model.name,"_data_issues.txt")
  }
  
  if (save2file) sink(file=reportname,append=F)
  #outfile <- file(reportname,"wt")
  
  cat("********  GGCMI Phase 3 file check summary report ********\n\n")
  cat(thisdate,"\n\n")
  cat("there are more detailed reports for specific aspects:\n")
  if (report_dir_web != "") {
    cat(fn.reportname2,"\n")
    cat(sim.reportname2,"\n")
    cat(data.reportname2,"\n")
  } else {
    cat(fn.reportname,"\n")
    cat(sim.reportname,"\n")
    cat(data.reportname,"\n")
  }
  
  return(reportnames)
}


do_test.filenames <- function(files, fn.reportname, save2file, thisdate, model.name) {
  
  fname.issues <- list()
  
  cat("/*=============================================================================================*/\n")
  cat("/*===================      FILE NAMING ISSUES     =============================================*/\n")
  cat("/*=============================================================================================*/\n")
  warnings <- errors <- 0
  error.types <- list("wrong file ending"=NULL,"inconsistent model/folder name"=NULL,"wrong GCM for climate"=NULL,
                      "unknown scenario"=NULL,"unknown soc setting"=NULL,"unknown sensitivty setting"=NULL,
                      "wrong variable"=NULL,"unknown crop"=NULL,"wrong irrigation setting"=NULL,
                      "wrong region"=NULL,"wrong time step"=NULL,"wrong start year"=NULL,"wrong end year"=NULL,"wrong bias adjustment"=NULL)
  for(fn in 1:1){
    test <- test.filename(files[fn], model.name)
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
  if (save2file) sink()
  
  if (save2file) sink(file=fn.reportname,append=F)
  cat("********  GGCMI Phase 3 file check report ********\n\n")
  cat(thisdate,"\n\n")
  if(length(fname.issues)>0){
    cat(unlist(fname.issues),sep="\n")
  } else {
    cat("no file naming issues detected.\n")
  }
  if (save2file) sink()

  stop()
}


do_test.file_set <- function(crops, irrigs, rcsps, socs, sens, gcms, vars, sims.reportname, save2file) {
  
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
  
  if (save2file) sink(file=sim.reportname,append=T)
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
      if (save2file) sink()
      if (save2file) sink(file=sim.reportname,append=F)
      cat("incomplete sets:\n")
      sims2[sims2!=1] <- "miss"
      sims2[sims2==1] <- "OK"
      print(sims2)
      if (save2file) sink()
      
    }
    #c(1:length(crops))[-mcrops]
  } else {
    cat("\nno issues detected\n\n")
  }
  if (save2file) sink()
  
}


do_test.files <- function(files, data.reportname, landseamask, save2file, thisdate) {
  
  data.issues <- list()
  
  cat("\n\n\n/*=============================================================================================*/\n")
  cat("/*===================      DATA RANGE and COVERAGE ISSUES      ================================*/\n")
  cat("/*=============================================================================================*/\n")
  warnings <- errors <- 0
  error.types <- list("variable isssues"=NULL,"number of dimensions"=NULL,"dimension names"=NULL,
                      "dimension definitions"=NULL,"units"=NULL,"data ranges"=NULL,
                      "data coverage"=NULL)
  for(fn in 1:length(files)){
    test <- test.file(files[fn], landseamask)
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
  if (save2file) sink()
  
  if (save2file) sink(file=data.reportname,append=F)
  cat("********  GGCMI Phase 3 data range and coverage check report ********\n\n")
  cat(thisdate,"\n\n")
  if(length(data.issues)>0){
    cat(unlist(data.issues),sep="\n")
  } else {
    cat("no data range and coverage issues detected.\n")
  }
  if (save2file) sink()
}




