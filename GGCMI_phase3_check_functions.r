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

test.filename <- function(fn){
  # <modelname>_<climate_forcing>_<bias_adjustment>_<climate_scenario>_<soc_scenario>_<sens_scenario>_<variable>-<crop>-<irrigation>_<region>_<timestep>_<start_year>_<end_year>.nc
  ending.f <- mname.f <- climate.f <- bias.f <- scen.f <- soc.f <- sens.f <- var.f <- crop.f <- irrig.f <- region.f <- timestep.f <- 
    starty.f <- endy.f <- NULL
  warnings <- errors <- 0
  # split up file name into elements
  # remove .nc parg
  bits <- unlist(strsplit(fn,"[.]"))
  ending <- bits[2]
  if(ending != "nc"){
    ending.f <- paste("  => WARNING: File ending is no '.nc' but",ending,"\n")
    warnings <- warnings + 1
  }
  # split rest of file name string into elements
  bits <- unlist(strsplit(bits[1],"_"))
  if(bits[1]!=tolower(args[1])){
    mname.f <- paste("  => WARNING: model name",bits[1],"not the same as folder name",args[1],"\n")
    warnings <- warnings + 1
  }
  if(!(bits[2]%in%tolower(gcms))){
    climate.f <- paste("  => ERROR: climate",bits[2],"not in set of GCMs\n")
    errors <- errors + 1
  }
  if(bits[3]!="w5e5"){
    bias.f <- paste("  => ERROR: bias_adjustment string",bits[3],"not 'w5e5'\n")
    errors <- errors + 1 
  }
  if(!(bits[4]%in%rcsps)){
    soc.f <- paste("  => ERROR: SSP/RCP scenario",bits[4],"not in set of scenarios",rcsps,"\n")
    errors <- errors + 1
  }
  if(!(bits[5]%in%tolower(socs))){
    soc.f <- paste("  => ERROR: soc scenario",bits[5],"not in set of soc scenarios\n")
    errors <- errors + 1
  }
  if(!(bits[6]%in%tolower(sens))){
    sens.f <- paste("  => ERROR: sens scenario",bits[6],"not in set of soc scenarios\n")
    errors <- errors + 1
  }
  bits2 <- unlist(strsplit(bits[7],"-"))
  if(length(bits2)!=3){
    var.f <- paste("  => ERROR: wrong number of elements in variable string:",bits[7],"\n")
    errors <- errors + 1
  }
  else{
    if(!(bits2[1]%in%vars)){
      var.f <- paste("  => ERROR: variable",bits2[1],"not in set of variables\n")
      errors <- errors + 1
    }
    if(!(bits2[2]%in%crops)){
      crops.f <- paste("  => WARNING: variable",bits2[2],"not in set of crops\n")
      warnings <- warnings + 1
    }
    if(!(bits2[3]%in%irrigs)){
      irrig.f <- paste("  => WARNING: variable",bits2[2],"not in set of crops\n")
      warnings <- warnings + 1
    }
  }
  if(!(bits[8]=="global")){
    region.f <- paste("  => ERROR: region",bits[8],"not 'global'\n")
    errors <- errors + 1
  }
  if(!(bits[9]=="annual")){
    timestep.f <- paste("  => ERROR: timestep",bits[9],"not 'annual'\n")
    errors <- errors + 1
  }
  if(!((bits[4]%in%rcsps[c(1,2)] & bits[10]==1850) | (bits[4]%in%rcsps[-c(1,2)] & bits[10]==2015))){
    starty.f <- paste("  => ERROR: startyear",bits[10],"not compatible with scenario",bits[4],"\n")
    errors <- errors + 1
  }
  if(!((bits[4]==rcsps[2] & bits[11]==2014) | (bits[4]%in%rcsps[-2] & bits[11]==2100))){
    endy.f <- paste("  => ERROR: endyear",bits[11],"not compatible with scenario",bits[4],"\n")
    errors <- errors + 1
  }
  list(ending.f=ending.f,mname.f=mname.f,climate.f=climate.f,bias.f=bias.f,scen.f=scen.f,soc.f=soc.f,sens.f=sens.f,
       var.f=var.f,crop.f=crop.f,irrig.f=irrig.f,region.f=region.f,timestep.f=timestep.f,starty.f=starty.f,endy.f=endy.f,
       warnings=warnings,errors=errors)
}

test.file <- function(fn){
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
      var.f <- paste("  => ERROR: variable uncorrectly named",var,"instead of",paste0(bits2[1],"_",bits2[2],".\n"))
      errors <- errors + 1
    }
    if(nc$var[[1]]$units!=units[index]){
      units.f <- paste(units.f,"  => ERROR: variable units incorrectly defined",nc$var[[1]]$units,"instead of '",units[index],"'\n")
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