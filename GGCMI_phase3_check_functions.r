failvalue <- -1

ranges <- list("yield" = c(0,100),
               "biom" = c(0,100),
               "cnyield" = c(0,100),
               "plantday" = c(1,366),
               "plantyear" = c(1600,2100),
               "harvyear" = c(1600,2100),
               "anthday" = c(0,365),
               "matyday" = c(0,365),
               "pirnreq" = c(0,1e6),
               "aet" = c(0,1e6),
               "soilmoist1m" = c(0,1e6),
               "transp" = c(0,1e6),
               "evap" = c(0,1e6),
               "runoff" = c(0,1e6),
               "rootm" = c(0,100),
               "tnrup" = c(0,1e4),
               "tnrin" = c(0,1e4),
               "tnrloss" = c(0,1e4),
               "n2oemis" = c(0,1e4),
               "n2emis" = c(0,1e4),
               "nleach" = c(0,1e4),
               "tcemis" = c(0,1e4),
               "ch4emis" = c(0,1e4))
              
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

target_units <- function(varcropirr_in) {
  # Get variable name
  var_in <- unlist(strsplit(varcropirr_in, "-"))[1]
  
  # Get units
  if (is.element(var_in, c("plantyear","harvyear"))) {
    units <- "calendar year"
  } else if (var_in == "plantday") {
    units <- "day of year"
  } else if (is.element(var_in, c("anthday", "matyday"))) {
    units <- "days from planting"
  } else if (is.element(var_in, c("tcemis", "ch4emis"))) {
    units <- "gC m-2 gs-1"
  } else if (is.element(var_in, c("n2oemis", "n2emis", "nleach"))) {
    units <- "gN m-2 gs-1"
  } else if (is.element(var_in, c("pirnreq", "aet", "transp", "evap", "runoff"))) {
    units <- "kg m-2 gs-1"
  } else if (var_in == "soilmoist1m") {
    units <- "kg m-3"
  } else if (is.element(var_in, c("tnrup", "tnrin", "tnrloss"))) {
    units <- "kgN ha-1 gs-1"
  } else if (is.element(var_in, c("yield", "biom", "rootm"))) {
    units <- "t ha-1 gs-1 (dry matter)"
  } else if (var_in == "cnyield") {
      units <- ""
  } else {
    stop(paste(var_in, "not recognized when trying to get units"))
  }
    
  return(units)
}

# Given the path to a file/directory, split the path into its component parts
# (each directory and file basename as separate items in a list)
# Taken from https://stackoverflow.com/questions/29214932/split-a-file-path-into-folder-names-vector
split_path <- function(path) {
  if (dirname(path) %in% c(".", path)) return(basename(path))
  return(c(basename(path), split_path(dirname(path))))
}


test.filename <- function(file_path, model.name, ignore){
  # <modelname>_<climate_forcing>_<bias_adjustment>_<climate_scenario>_<soc_scenario>_<sens_scenario>_<variable>-<crop>-<irrigation>_<region>_<timestep>_<start_year>_<end_year>.nc
  ending.f <- mname.f <- climate.f <- bias.f <- scen.f <- soc.f <- sens.f <- var.f <- crop.f <- irrig.f <- region.f <- timestep.f <- 
    years.f <- NULL
  warnings <- errors <- 0
  
  # Get filename and directory
  fn <- basename(file_path)
  dir_path <- dirname(normalizePath(file_path))
  
  # split up file name into elements
  bits <- unlist(strsplit(fn,"[.]"))
  ending <- bits[length(bits)]
  ending_bad <- is.na(ending) | !(ending %in% c("nc", "nc4"))
  if (ending_bad) {
    if (is.na(ending)) {
      ending.f <- "  => Error: File has no extension. Halting check of this filename.\n"
      errors <- errors + 1
    }
    else if (!(ending %in% c("nc", "nc4"))) {
      ending.f <- paste0("  => Error: File extension '.", ending, "' instead of '.nc'. Halting check of this filename.\n")
      errors <- errors + 1
    }
    else {
      ending.f <- "  => Error: Problem parsing extension. Halting check of this filename.\n"
      errors <- errors + 1
    }
  }
  else {
    if (length(bits) > 2) {
      ending.f <- "  => WARNING: Filename has multiple periods; it should just have one (*.nc)\n"
      warnings <- warnings + 1
    }
    if (ending == "nc4") {
      ending.f <- paste(ending.f, "  => WARNING: File extension should be '.nc', not '.nc4'\n")
      warnings <- warnings + 1
    }
    
    # split rest of file name string into elements
    bits <- unlist(strsplit(bits[1],"_"))
    bit_model <- bits[1]
    bit_gcm <- bits[2]
    bit_bc <- bits[3]
    bit_scen <- bits[4]
    bit_soc <- bits[5]
    bit_sens <- bits[6]
    fn_var <- bits[7]
    bit_region <- bits[8]
    bit_timestep <- bits[9]
    
    # Get directory structure
    dir_bits <- split_path(dir_path)
    dir_bit_gcm <- dir_bits[2]
    dir_bit_model <- dir_bits[5]
    
    # Check that filename has correct value for <modelname>
    if (is.na(bit_model)) {
      mname.f <- "  => WARNING: Failed to parse <modelname> from filename\n"
      warnings <- warnings + 1
    }
    else if (bit_model != tolower(model.name)) {
      if (tolower(bit_model) == tolower(model.name)) {
        mname.f <- "  => WARNING: <modelname> in filename should be all lowercase\n"
      } 
      else {
        mname.f <- paste0("  => WARNING: <modelname> in filename (", bit_model,
                          ") does not match value provided in script (", model.name,")\n")
      }
      warnings <- warnings + 1
    }
    
    # Check that directory structure has correct value for <modelname>
    if (is.na(dir_bit_model)) {
      mname.f <- paste(mname.f, "  => WARNING: Failed to find <modelname> in directory structure\n")
    }
    else if (dir_bit_model != tolower(model.name)) {
      if (tolower(dir_bit_model) == tolower(model.name)) {
        # model name in directory should not be converted to all lowercase, but can be a mixed bag
        # mname.f <- paste(mname.f, "  => WARNING: <modelname> in directory structure should be all lowercase\n")
      } else {
        mname.f <- paste0(mname.f, "  => WARNING: directory that should be '", model.name, 
                         "' is instead '", dir_bit_model, "'\n")
        warnings <- warnings + 1
      }
    }
    
    # Check that filename has correct value for <climate_forcing>
    if (is.na(bit_gcm)) {
      climate.f <- "  => ERROR: Failed to parse <climate_forcing> from filename\n"
      errors <- errors + 1
    }
    else if (!(bit_gcm %in% tolower(gcms))) {
      if (tolower(bit_gcm) %in% tolower(gcms)) {
        climate.f <- "  => WARNING: <climate_forcing> in filename should be all lowercase\n"
        warnings <- warnings + 1
      } else {
        climate.f <- paste("  => ERROR:", bit_gcm, "not in set of GCMs\n")
        errors <- errors + 1
      }
    }
    
    # Check that directory structure has correct value for <climate_forcing>
    if (is.na(dir_bit_gcm)) {
      climate.f <- paste(climate.f, "  => WARNING: Failed to find <climate_forcing> in directory structure\n")
    }
    else if (dir_bit_gcm != tolower(bit_gcm)) {
      if (tolower(dir_bit_gcm) == tolower(bit_gcm)) {
        climate.f <- paste0(climate.f, "   => WARNING: <climate_forcing> in directory structure should be all lowercase\n")
      } else {
        climate.f <- paste0(climate.f, "   => WARNING: directory that should be '", bit_gcm, 
                          "' is instead '", dir_bit_gcm, "'\n")
      }
      warnings <- warnings + 1
    }
    
    # Check that bias adjustment string in the filename is correct
    if (is.na(bit_bc)) {
      bias.f <- "  => ERROR: Failed to parse <bias_adjustment> from filename\n"
      errors <- errors + 1
    }
    else if(bit_bc != "w5e5"){
      if (tolower(bit_bc) != "w5e5") {
        bias.f <- "  => WARNING: <bias_adjustment> in filename should be all lowercase\n"
        warnings <- warnings + 1
      } else {
        bias.f <- paste("  => ERROR: bias_adjustment string in filename is", bit_bc, "not 'w5e5'\n")
        errors <- errors + 1
      }
    }
    
    # Check that SSP/RCP scenario is valid
    if (is.na(bit_scen)) {
      scen.f <- "  => ERROR: Failed to parse <climate_scenario> from filename\n"
      errors <- errors + 1
    }
    else if(!(bit_scen %in% tolower(rcsps))){
      if (tolower(bit_scen) %in% tolower(rcsps)) {
        soc.f <- "  => WARNING: <climate_scenario> in filename should be all lowercase\n"
        warnings <- warnings + 1
      } else {
        soc.f <- paste("  => ERROR:", bit_scen, "not in set of SSP/RCP scenarios\n")
        errors <- errors + 1
      }
    }
    
    # Check that socioeconomic scenario is valid
    if (is.na(bit_soc)) {
      soc.f <- "  => ERROR: Failed to parse <soc_scenario> from filename\n"
      errors <- errors + 1
    }
    else if(!(bit_soc %in% tolower(socs))){
      if (tolower(bit_soc) %in% tolower(socs)) {
        soc.f <- "  => WARNING: <soc_scenario> in filename should be all lowercase\n"
        warnings <- warnings + 1
      } else {
        soc.f <- paste("  => ERROR:", bit_soc, "not in set of socioeconomic scenarios\n")
        errors <- errors + 1
      }
    }
    
    # Check that sensitivity scenario is valid
    if (is.na(bit_sens)) {
      sens.f <- "  => ERROR: Failed to parse <soc_scenario> from filename\n"
      errors <- errors + 1
    }
    else if(!(bit_sens %in% tolower(sens))){
      if (tolower(bit_sens) %in% tolower(sens)) {
        sens.f <- "  => WARNING: <sens_scenario> in filename should be all lowercase\n"
        warnings <- warnings + 1
      } else {
        sens.f <- paste("  => ERROR:", bit_sens, "not in set of sensitivity scenarios\n")
        errors <- errors + 1
      }
    }
    
    # Get and check variable in filename
    bits_varcropirr <- unlist(strsplit(fn_var,"-"))
    bit_var <- bits_varcropirr[1]
    bit_crop <- bits_varcropirr[2]
    bit_irr <- bits_varcropirr[3]
    if (length(bits_varcropirr) != 3) {
      var.f <- paste("  => ERROR: wrong number of elements in variable string",fn_var,"\n")
      errors <- errors + 1
    }
    else{
      # TODO: Why is this an error for variable but a warning for the other two strings?
      if (!(bit_var %in% vars)) {
        var.f <- paste("  => ERROR:", bit_var, "not in set of variables\n")
        errors <- errors + 1
      }
      if (!(bit_crop %in% crops)) {
        crop.f <- paste("  => WARNING:", bit_crop, "not in set of crops\n")
        warnings <- warnings + 1
      }
      if (!(bit_irr %in% irrigs)) {
        irrig.f <- paste("  => ERROR:", bit_irr, "not in set of irrigations\n")
        errors <- errors + 1
      }
    }
    
    # Check that area coverage element is correct
    if (is.na(bit_region)) {
      region.f <- "  => ERROR: Failed to parse <region> from filename\n"
      errors <- errors + 1
    }
    else if (bit_region != "global"){
      region.f <- paste0("  => ERROR: region is '", bit_region, "' instead of 'global'\n")
      errors <- errors + 1
    }
    
    # Check that temporal resolution element is correct
    if (is.na(bit_timestep)) {
      timestep.f <- "  => ERROR: Failed to parse <timestep> from filename\n"
      errors <- errors + 1
    }
    else if (bit_timestep != "annual" && !(bit_timestep == "monthly" && bit_var == "soilmoist1m")){
      timestep.f <- paste0("  => ERROR: timestep is '", bit_timestep, "' instead of 'annual'\n")
      errors <- errors + 1
    }
    
    # Check that time span is correct
    if (!ignore$years) {
      if (is.na(bit_scen)) {
        years.f <- "  => ERROR: Can't check time span: Failed to parse RCP/SSP scenario from filename\n"
        errors <- errors + 1
      }
      else {
        if (!(bit_scen %in% rcsps)) {
          years.f <- "  => ERROR: Can't check time span: parsed RCP/SSP scenario not in list provided\n"
          errors <- errors + 1
        }
        else {
          startyear <- bits[10]
          endyear <- bits[11]
          if (is.na(startyear) | is.na(endyear)) {
            years.f <- "  => ERROR: Can't check time span: Failed to parse years from filename\n"
            errors <- errors + 1
          }
          else {
            
            # Get target start year
            if (bit_scen %in% c("picontrol", "historical")) {
              target_startyear <- 1850
            }
            else if (bit_scen %in% c("ssp126", "ssp585", "ssp370")) {
              target_startyear <- 2015
            } else {
              target_startyear <- NA
            }
            
            # Get target end year
            if (bit_scen %in% c("historical")) {
              target_endyear <- 2014
            }
            else if (bit_scen %in% c("picontrol", "ssp126", "ssp585", "ssp370")) {
              target_endyear <- 2100
            } else {
              target_endyear <- NA
            }
            
            # Check time span
            if (is.na(target_startyear) | is.na(target_endyear)) {
              years.f <- paste("  => ERROR: Can't check time span: Target start and/or end year(s) not provided", 
                               "for RCP/SSP scenario", bit_scen, "\n")
              errors <- errors + 1
            }
            else {
              if (startyear != target_startyear){
                years.f <- paste0("  => ERROR: startyear ", startyear, " not compatible with RCP/SSP scenario ", bit_scen,
                                  " (should be ", target_startyear, ")\n")
                errors <- errors + 1
              }
              if (endyear != target_endyear){
                years.f <- paste0(years.f, "  => ERROR: endyear ", endyear, " not compatible with RCP/SSP scenario ", bit_scen,
                                  " (should be ", target_endyear, ")\n")
                errors <- errors + 1
              }
            }
          }
        }
      }
    }
  }
  
  # Save result
  list(ending.f = ending.f,
       mname.f = mname.f,
       climate.f = climate.f,
       bias.f = bias.f,
       scen.f = scen.f,
       soc.f = soc.f,
       sens.f = sens.f,
       var.f = var.f,
       crop.f = crop.f,
       irrig.f = irrig.f,
       region.f = region.f,
       timestep.f = timestep.f,
       years.f = years.f,
       warnings = warnings,
       errors = errors)
}

test.file <- function(fn, landseamask){
  var.f <- ndim.f <- dimname.f <- dimdef.f <- units.f <- range.f <- cover.f <- timespan.f <- missval.f <- NULL  
  warnings <- errors <- 0

  bits <- unlist(strsplit(fn,"[.]"))
  # split first part of file name string into elements
  bits <- unlist(strsplit(bits[1],"_"))
  bits2 <- unlist(strsplit(bits[7],"-"))
  index <- which(vars==bits2[1])
  if(length(index)>0){
    nc <- nc_open(fn)
    
    #=#=#=#=#=#=#=#=#=#=#=#=#=#=
    # Check metadata
    #=#=#=#=#=#=#=#=#=#=#=#=#=#=
    var <- names(nc$var)[1]
    target_varname <- paste0(bits2[1],"-",bits2[2],"-",bits2[3])
    if(var != target_varname){
      var.f <- paste("  => ERROR: variable incorrectly named",var,"instead of",target_varname,".\n")
      errors <- errors + 1
    }
    if(nc$var[[1]]$units!=target_units(var)){
      units.f <- paste0(units.f,"  => ERROR: variable units incorrectly defined as '",nc$var[[1]]$units,
                        "' instead of '",target_units(var),"'\n")
      errors <- errors + 1
    }
    # precision issue, floating point comparison is unreliable: 1e20 != 1e20
    #if(nc$var[[1]]$missval!=1e20){
      if (abs(nc$var[[1]]$missval*1e-20 - 1) > 1e-6) {
        #missval.f <- paste0(range.f,"   => WARNING: R reads missing value as '",nc$var[[1]]$missval,"' instead of '1e20'.\n")
        #warnings <- warnings + 1
      #} else {
        missval.f <- paste0(range.f,"   => ERROR: missing value incorrectly defined as '",nc$var[[1]]$missval,
                            "' instead of '1e20'\n")
        errors <- errors + 1
      }
    #}
    
    #=#=#=#=#=#=#=#=#=#=#=#=#=#=
    # Check dimensions
    #=#=#=#=#=#=#=#=#=#=#=#=#=#=
    
    # Check for correct number of dimensions
    if(nc$ndims!=3){
      ndim.f <- paste("  => ERROR: wrong number of dimensions",nc$ndims,"\n")
      errors <- errors + 1
    }
    
    # Check lat
    if(!("lat" %in% names(nc$dim))){
      dimname.f <- paste(dimname.f,"  => ERROR: latitude dimension 'lat' missing")
      errors <- errors + 1
    } else {
      lat <- ncvar_get(nc,"lat")
      if(!all(lat==landseamask$lat)){
        dimdef.f <- paste(dimdef.f,"  => ERROR: latitude dimension is incorrectly defined, ranging from",lat[1],"to",
                          lat[length(lat)],"by",lat[2]-lat[1],"\n")
        errors <- errors + 1
      }
      if(nc$dim$lat$units!="degrees_north"){
        dimdef.f <- paste(dimdef.f,"  => ERROR: latitude units incorrectly defined",nc$dim$lat$units,"instead of 'degrees_north'\n")
        errors <- errors + 1
      }
    }
    
    # Check lon
    if(!("lon" %in% names(nc$dim))){
      dimname.f <- paste(dimname.f,"  => ERROR: longitude dimension 'lon' missing")
      errors <- errors + 1
    } else {
      lon <- ncvar_get(nc,"lon")
      if(!all(lon==landseamask$lon)){
        dimdef.f <- paste(dimdef.f,"  => ERROR: longitude dimension is incorrectly defined, ranging from",lon[1],"to",
                          lon[length(lon)],"by",lon[2]-lon[1],"\n")
        errors <- errors + 1
      }
      if(nc$dim$lon$units!="degrees_east"){
        dimdef.f <- paste(dimdef.f,"  => ERROR: longitude units incorrectly defined",nc$dim$lon$units,"instead of 'degrees_east'\n")
        errors <- errors + 1
      }
    }
    
    # Check time
    if(!("time" %in% names(nc$dim))){
      dimname.f <- paste(dimname.f,"  => ERROR: time dimension 'time' missing")
      errors <- errors + 1
    } else {
      time <- ncvar_get(nc,"time")
      if(!all(lat==landseamask$lat)){
        dimdef.f <- paste(dimdef.f,"  => ERROR: latitude dimension is incorrectly defined, ranging from",lat[1],"to",
                          lat[length(lat)],"by",lat[2]-lat[1],"\n")
        errors <- errors + 1
      }
      if (unlist(strsplit(var, "-"))[1] == "soilmoist1m") {
        since_units <- "months"
      } else {
        since_units <- "growing seasons"
      }
      if(nc$dim$time$units!="growing seasons since 1661-01-01, 00:00:00"){
        if(nc$dim$time$units!="growing seasons since 1661-01-01 00:00:00"){
          dimdef.f <- paste0(dimdef.f,"  => WARNING: time units incorrectly defined as '",nc$dim$time$units,"' instead of '",
                             since_units," since 1661-01-01, 00:00:00' (commma missing)\n")
          warnings <- warnings + 1
        } else {
          dimdef.f <- paste0(dimdef.f,"  => ERROR: time units incorrectly defined as '",nc$dim$time$units,"' instead of '",
                             since_units," since 1661-01-01, 00:00:00'\n")
          errors <- errors + 1
        }
      }
    }
    fn_years <- tail(unlist(strsplit(unlist(strsplit(basename(fn), ".", fixed=TRUE))[1], "_")), 2)
    if (!identical(c(1,2), as.numeric(grep("[0-9][0-9][0-9][0-9]", fn_years)))) {
      timespan.f <- paste0(timespan.f,"  => ERROR: unable to parse time span from filename for checking size of time dimension\n")
      errors <- errors + 1
    } else {
      y1 <- as.numeric(fn_years[1])
      yN <- as.numeric(fn_years[2])
      Ntimesteps_target <- yN - y1 + 1
      if (unlist(strsplit(var, "-"))[1] == "soilmoist1m") {
        Ntimesteps_target = Ntimesteps_target * 12
      }
      Ntimesteps <- dim(ncvar_get(nc,"time"))
      if (Ntimesteps != Ntimesteps_target) {
        timespan.f <- paste0(timespan.f,"  => ERROR: time dimension has size ", Ntimesteps, " instead of ", Ntimesteps_target, 
                             " expected based on filename\n")
        errors <- errors + 1
      }
    }

    #=#=#=#=#=#=#=#=#=#=#=#=#=#=
    # Check variable data
    #=#=#=#=#=#=#=#=#=#=#=#=#=#=
    
    # Read
    data <- ncvar_get(nc,var)
    test1 <- data[,,1]
    test2 <- data[,,dim(data)[3]]
    data.r <- range(data,na.rm=T)
    
    # Check for match to land/ocean mask
    if(!all(is.finite(test1[landseamask$mask==1]))){
      cover.f <- paste(cover.f,"  => WARNING: not all land with valid values in first time step (",
                       length(which(!is.finite(test1[is.finite(landseamask$mask)]))),"of",length(which(is.finite(landseamask$mask))),")\n")
      warnings <- warnings + 1
    }
    if(!all(!is.finite(test1[landseamask$mask==0]))){
      cover.f <- paste(cover.f,"  => ERROR: not all ocean with invalid values in first time step (",
                       length(which(is.finite(test1[!is.finite(landseamask$mask)]))),"of",length(which(!is.finite(landseamask$mask))),")\n")
      errors <- errors + 1
    }
    if(!all(is.finite(test2[landseamask$mask==1]))){
      cover.f <- paste(cover.f,"  => WARNING: not all land with valid values in last time step (",
                       length(which(!is.finite(test2[is.finite(landseamask$mask)]))),"of",length(which(is.finite(landseamask$mask))),")\n")
      warnings <- warnings + 1
    }
    if(!all(!is.finite(test2[landseamask$mask==0]))){
      cover.f <- paste(cover.f,"  => ERROR: not all ocean with invalid values in last time step (",
                       length(which(is.finite(test2[!is.finite(landseamask$mask)]))),"of",length(which(!is.finite(landseamask$mask))),")\n")
      errors <- errors + 1
    }
    
    # Check for valid values
    if(min(data.r[data.r!=failvalue]) < ranges[[index]][1] | max(data.r[data.r!=failvalue]) > ranges[[index]][2]){
      range.f <- paste(range.f, "  => WARNING: data points (range:",paste(data.r[data.r!=failvalue],collapse=" "),
                       ") outside valid range", paste(ranges[[index]],collapse=" "), "\n")
      warnings <- warnings + 1
    }
    
  } else {
    var.f <- paste("  => ERROR: Variable",bits2[1],"unknown\n")
    errors <- errors + 1
  }

  list(var.f = var.f,
       ndim.f = ndim.f,
       dimname.f = dimname.f,
       dimdef.f = dimdef.f,
       units.f = units.f,
       range.f = range.f,
       cover.f = cover.f,
       timespan.f = timespan.f,
       missval.f = missval.f,
       warnings = warnings,
       errors = errors)
}


setup_reports <- function(report_dir, report_dir_web, save2file, thisdate, model.name) {
  
  # If report_dir not specified, set it to working_dir
  if (report_dir == "") {
    report_dir = paste0(working_dir, "/../")
  }
  
  # delete old reports
  unlink(paste0(report_dir,model.name,"*"))
  reportnames <- list(
    "summary" = paste0(report_dir,model.name,"_summary.txt"),
    "fn" = paste0(report_dir,model.name,"_filename_issues.txt"), 
    "sim" = paste0(report_dir,model.name,"_simulations_missing.txt"), 
    "data" = paste0(report_dir,model.name,"_data_issues.txt"))
  
  if (save2file) sink(file=reportnames$summary,append=F)

  cat("\n\n********  GGCMI Phase 3 file check summary report ********\n\n")
  cat("there are more detailed reports for specific aspects:\n")
  if (report_dir_web != "") {
    cat(paste0(report_dir_web,model.name,"_filename_issues.txt"),"\n")
    cat(paste0(report_dir_web,model.name,"_simulations_missing.txt"),"\n")
    cat(paste0(report_dir_web,model.name,"_data_issues.txt"),"\n")
  } else {
    cat(reportnames$fn,"\n")
    cat(reportnames$sim,"\n")
    cat(reportnames$data,"\n")
  }
  
  # stop reporting
  if (save2file) sink()
  
  return(reportnames)
}


do_test.filenames <- function(files, reportnames, save2file, thisdate, model.name, ignore) {
  
  fname.issues <- list()
  
  if (save2file) sink(file=reportnames$fn,append=F)
  cat("/*=============================================================================================*/\n")
  cat("/*===================      FILE NAMING ISSUES     =============================================*/\n")
  cat("/*=============================================================================================*/\n")
  cat(thisdate,"\n\n")
  if (ignore$years) {
    cat("WARNING: Not checking years in filenames\n\n")
  }
  warnings <- errors <- 0
  error.types <- list("wrong file ending"=NULL,
                      "inconsistent model/folder name"=NULL,
                      "wrong GCM for climate"=NULL,
                      "unknown scenario"=NULL,
                      "unknown soc setting"=NULL,
                      "unknown sensitivity setting"=NULL,
                      "wrong variable"=NULL,
                      "unknown crop"=NULL,
                      "wrong irrigation setting"=NULL,
                      "wrong region"=NULL,
                      "wrong time step"=NULL,
                      "wrong years"=NULL,
                      "wrong bias adjustment"=NULL)
  for(fn in 1:length(files)){
    test <- test.filename(files[fn], model.name, ignore)
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
    if(!is.null(test$years.f)) error.types[[12]] <- c(error.types[[12]],fn)
    if(!is.null(test$bias.f)) error.types[[13]] <- c(error.types[[13]],fn)
    collected <- paste0(if(!is.null(test$ending.f))test$ending.f,
                        if(!is.null(test$mname.f))test$mname.f,
                        if(!is.null(test$climate.f))test$climate.f,
                        if(!is.null(test$bias.f))test$bias.f,
                        if(!is.null(test$scen.f))test$scen.f,
                        if(!is.null(test$soc.f))test$soc.f,
                        if(!is.null(test$sens.f))test$sens.f,
                        if(!is.null(test$var.f))test$var.f,
                        if(!is.null(test$crop.f))test$crop.f,
                        if(!is.null(test$irrig.f))test$irrig.f,
                        if(!is.null(test$region.f))test$region.f,
                        if(!is.null(test$timestep.f))test$timestep.f,
                        if(!is.null(test$years.f))test$years.f)
    if(length(collected)>0)
      fname.issues[length(fname.issues)+1] <- paste0("file naming issues (",test$warnings," warnings; ",test$errors,
                                                     " errors) with ",files[fn],"\n",collected)
  }
  
  # Print messages for every netCDF to detailed output file
  if(length(fname.issues)>0){
    cat(unlist(fname.issues),sep="\n")
  } else {
    cat("no file naming issues detected.\n")
  }
  
  # Print selection to summary file
  if (save2file) {
    sink()
    sink(file=reportnames$summary, append=T)
  }
  cat("\n\n********  GGCMI Phase 3 filename check report ********\n\n")
  if (ignore$years) {
    cat("WARNING: Not checking years in filenames\n\n")
  }
  if(length(fname.issues)>0){
    cat(length(fname.issues),"file names issues in ",length(files)," files, with ",warnings,"Warnings and ",errors,
        "errors.\n\n")
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
        cat(counter,": ",names(error.types)[i],": in ",length(error.types[[i]])," files, e.g. ",files[error.types[[i]][1]],
            "\n",sep="")
        counter <- counter +1 
      }
    }
  }
  if (save2file) sink()
}


do_test.file_set <- function(crops, irrigs, rcsps, socs, sens, gcms, vars, sim.reportname, save2file, ignore) {
  
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
                if (ignore$years) {
                  fn <- paste0(tolower(gcms[gcm]), "/", 
                               rcsps[rcsp], "/", 
                               crops[crop], "/", 
                               model.name,"_",
                               tolower(gcms[gcm]),
                               "_w5e5_",
                               rcsps[rcsp],"_",
                               socs[soc],"_",
                               sens[sen],"_",
                               vars[var],"-",
                               crops[crop],"-",
                               irrigs[irrig],
                               "_global_annual_*.nc")
                  does_exist <- !identical(Sys.glob(fn), character(0))
                } else {
                  fn <- paste0(tolower(gcms[gcm]), "/", 
                               rcsps[rcsp], "/", 
                               crops[crop], "/", 
                               model.name,"_",
                               tolower(gcms[gcm]),
                               "_w5e5_",
                               rcsps[rcsp],"_",
                               socs[soc],"_",
                               sens[sen],"_",
                               vars[var],"-",
                               crops[crop],"-",
                               irrigs[irrig],
                               "_global_annual_",
                               ifelse(rcsp<3,1850,2015), "_",
                               ifelse(rcsp==2,2014,2100),".nc")
                  does_exist <- file.exists(fn)
                }
                if(does_exist){
                  sims[crop,irrig,rcsp,soc,sen,gcm,var] <- 1
                }
              }
            }
          }
        }
      }
    }
  }
  
  if (save2file) sink(file=reportnames$sim,append=T)
  cat("/*=============================================================================================*/\n")
  cat("/*===================      MISSING OUTPUTS        =============================================*/\n")
  cat("/*=============================================================================================*/\n")
  cat(thisdate, "\n\n")
  
  if(!all(!is.na(sims))){
    mcrops <- mirrigs <- mrcsps <- msocs <- msens <- mgcms <- mvars <- NULL
    
    # Get list of missing crops
    for(crop in 1:length(crops)){
      if(all(is.na(sims[crop,,,,,,]))){
        mcrops <- c(mcrops,crop)
      }
    }
    if(length(mcrops)>0){
      cat("missing crops (",length(mcrops),"of",length(crops),"):",paste(crops[mcrops],collapse=", "),"\n")
    }
    
    # Get list of missing irrigation values
    for(irrig in 1:length(irrigs)){
      if(all(is.na(sims[,irrig,,,,,]))){
        mirrigs <- c(mirrigs,irrig)
      }
    }
    if(length(mirrigs)>0){
      cat("missing irrigation settings (",length(mirrigs),"of",length(irrigs),"):",paste(irrigs[mirrigs],collapse=", "),"\n")
    }
    
    # Get list of missing climate scenarios
    for(rcsp in 1:length(rcsps)){
      if(all(is.na(sims[,,rcsp,,,,]))){
        mrcsps <- c(mrcsps,rcsp)
      }
    }
    if(length(mrcsps)>0){
      cat("missing climate scenarios (",length(mrcsps),"of",length(rcsps),"):",paste(rcsps[mrcsps],collapse=", "),"\n")
    }
    
    # Get list of missing socioeconomic settings
    for(soc in 1:length(socs)){
      if(all(is.na(sims[,,,soc,,,]))){
        msocs <- c(msocs,soc)
      }
    }
    if(length(msocs)>0){
      cat("missing soc settings (",length(msocs),"of",length(socs),"):",paste(socs[msocs],collapse=", "),"\n")
    }
    
    # Get list of missing sensitivity scenarios
    for(sen in 1:length(sens)){
      if(all(is.na(sims[,,,,sen,,]))){
        msens <- c(msens,sen)
      }
    }
    if(length(msens)>0){
      cat("missing sensitivity scenarios (",length(msens),"of",length(sens),"):",paste(sens[msens],collapse=", "),"\n")
    }
    
    # Get list of missing GCMs
    for(gcm in 1:length(gcms)){
      if(all(is.na(sims[,,,,,gcm,]))){
        mgcms <- c(mgcms,gcm)
      }
    }
    if(length(mgcms)>0){
      cat("missing gcms (",length(mgcms),"of",length(gcms),"):",paste(gcms[mgcms],collapse=", "),"\n")
    }
    
    # Get list of missing variables
    for(var in 1:length(vars)){
      if(all(is.na(sims[,,,,,,var]))){
        mvars <- c(mvars,var)
      }
    }
    if(length(mvars)>0){
      cat("missing variables (",length(mvars),"of",length(vars),"):",paste(vars[mvars],collapse=", "),"\n")
    }
    
    
    # partially missing things
    sims2 <- sims[if(!is.null(mcrops))c(1:length(crops))[-mcrops] else 1,
                  if(!is.null(mirrigs))c(1:length(irrigs))[-mirrigs] else 1,
                  if(!is.null(mrcsps))c(1:length(rcsps))[-mrcsps] else 1,
                  if(!is.null(msocs))c(1:length(socs))[-msocs] else 1,
                  if(!is.null(msens))c(1:length(sens))[-msens] else 1,
                  if(!is.null(mgcms))c(1:length(gcms))[-mgcms] else 1,
                  if(!is.null(mvars))c(1:length(vars))[-mvars] else 1]
    
    if(!all(sims2==1)){
      cat("incomplete sets:\n")
      sims2[sims2!=1] <- "miss"
      sims2[sims2==1] <- "OK"
      print(sims2)
      if (save2file) {
        sink()
        sink(file=reportnames$summary, append=T)
      }
      cat("incomplete sets:",length(sims2[sims2!=1]),"of",length(sims2),"see",reportnames$sim,"for details.\n")
      
    }
  } else {
    cat("\nno issues detected\n\n")
  }
  if (save2file) sink()
  
}


do_test.files <- function(files, reportnames, landseamask, save2file, thisdate) {
  
  data.issues <- list()
  
  if (save2file) sink(file=reportnames$data,append=F)
  cat("/*=============================================================================================*/\n")
  cat("/*===================      DATA RANGE and COVERAGE ISSUES      ================================*/\n")
  cat("/*=============================================================================================*/\n")
  cat(thisdate, "\n\n")
  cat(paste("\nReading", length(files), "files...\n"))
  warnings <- errors <- 0
  error.types <- list("variable issues"=NULL, 
                      "number of dimensions"=NULL, 
                      "dimension names"=NULL,
                      "dimension definitions"=NULL, 
                      "units"=NULL, 
                      "data ranges"=NULL,
                      "data coverage"=NULL, 
                      "time span"=NULL, 
                      "missing value"=NULL)
  for(fn in 1:10){
    
    cat(paste0(fn, "..."))
    if (fn%%10 == 0) cat("\n")
    
    test <- test.file(files[fn], landseamask)
    warnings <- warnings + test$warnings
    errors <- errors + test$errors
    if(!is.null(test$var.f)) error.types[[1]] <- c(error.types[[1]],fn)
    if(!is.null(test$ndim.f)) error.types[[2]] <- c(error.types[[2]],fn)
    if(!is.null(test$dimname.f)) error.types[[3]] <- c(error.types[[3]],fn)
    if(!is.null(test$dimdef.f)) error.types[[4]] <- c(error.types[[4]],fn)
    if(!is.null(test$units.f)) error.types[[5]] <- c(error.types[[5]],fn)
    if(!is.null(test$range.f)) error.types[[6]] <- c(error.types[[6]],fn)
    if(!is.null(test$cover.f)) error.types[[7]] <- c(error.types[[7]],fn)
    if(!is.null(test$timespan.f)) error.types[[8]] <- c(error.types[[8]],fn)
    if(!is.null(test$missval.f)) error.types[[9]] <- c(error.types[[9]],fn)
    
    collected <- paste0(if(!is.null(test$var.f))test$var.f,
                        if(!is.null(test$ndim.f))test$ndim.f,
                        if(!is.null(test$dimname.f))test$dimname.f,
                        if(!is.null(test$dimdef.f))test$dimdef.f,
                        if(!is.null(test$units.f))test$units.f,
                        if(!is.null(test$range.f))test$range.f,
                        if(!is.null(test$cover.f))test$cover.f,
                        if(!is.null(test$timespan.f))test$timespan.f,
                        if(!is.null(test$missval.f))test$missval.f)
    if(length(collected)>0)
      data.issues[length(data.issues)+1] <- paste0("data range and coverage issues (",test$warnings," warnings; ",test$errors,
                                                   " errors) with ",files[fn],"\n",collected)
  }
  
  # Print messages for every netCDF to detailed output file
  cat("\n\n")
  if(length(data.issues)>0){
    cat(unlist(data.issues),sep="\n")
  } else {
    cat("no data range or coverage issues detected.\n")
  }
  
  # Print selection to summary file
  if (save2file) {
    sink()
    if (save2file) sink(file=reportnames$summary, append=T)
  }
  cat("\n\n********  GGCMI Phase 3 data range and coverage check report ********\n\n")
  cat("\n\n")
  if(length(data.issues)>0){
    cat(length(data.issues),"data issues in ",length(files)," files, with ",warnings,"Warnings and ",errors,"errors.\n\n")
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
        cat(counter,": ",names(error.types)[i],": in ",length(error.types[[i]])," files, e.g. ",files[error.types[[i]][1]],
            "\n",sep="")
        counter <- counter +1 
      }
    }
  } else {
    cat("no data range or coverage issues detected.\n")
  }
  if (save2file) sink()
}




