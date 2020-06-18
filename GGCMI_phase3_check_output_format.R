require(ncdf4)

save2file <- FALSE

# Paths
# path_AgMIP.output <- "/project2/ggcmi/AgMIP.output/"
path_AgMIP.output <- "/Volumes/Reacher/GGCMI/runs_ggcmi_3b/tests/test_shortspinup/output-2020-06-15-202043-pp"
landseamask_file <- "/Volumes/Reacher/GGCMI/inputs/phase3/ISIMIP3/landseamask/landseamask_no_antarctica.nc"
report_dir <- "" # Set to "" to use working directory (top-level directory of outputs)
report_dir_web <- "" # Set to "" to ignore
ggcmi_function_file <- "/Users/Shared/GGCMI/inputs/phase3/ISIMIP3/_MATLAB_ISIMIP3/phase3_quality_check/GGCMI_phase3_check_functions.r"

# settings and definitions ####
model.name.orig <- "lpj-guess"
crops <- c("mai","ri1","ri2","soy","swh","wwh","mil","sor","bea")
irrigs <- c("firr","noirr")
rcsps <- c("picontrol","historical","ssp126","ssp585")
socs <- c("2015soc")
sens <- c("default","2015co2")
gcms <- c("GFDL-ESM4","IPSL-CM6A-LR","MPI-ESM1-2-HR","MRI-ESM2-0","UKESM1-0-LL")
vars <- c("yield","biom","cnyield","plantday","plantyear","anthday","matyday","pirrww","aet","soilmoist1m",
          "transp","evap","runoff","rootm","tnrup","tnrin","tnrloss","n2oemis","n2emis","nleach","tcemis","ch4emis")


#######################
# Set up
#######################
source(ggcmi_function_file)
thisdate <- date()

# get GGCM folder name passed as argument to script call
args <- commandArgs(trailingOnly = TRUE)
if (length(args)==0) {
  # stop("GGCM folder name must be supplied (input file).", call.=FALSE)
  args = c(model.name.orig)
}

# Get and change working directory,
# ensuring that the last character of path_AgMIP.output is /
if (substr(path_AgMIP.output, nchar(path_AgMIP.output), nchar(path_AgMIP.output)) != "/") {
  path_AgMIP.output <- paste0(path_AgMIP.output, "/")
}
working_dir <- paste0(path_AgMIP.output,args[1],"/phase3b")
if (!dir.exists(working_dir)) {
  stop(paste("working_dir does not exist:", working_dir))
}
setwd(working_dir)

# Import landseamask
landseamask <- readmask.nc(landseamask_file)

# Set up reports
reportnames <- setup_reports(report_dir, report_dir_web, save2file, thisdate)


#######################
# Test filenames
#######################
files <- dir(recursive=TRUE, include.dirs=FALSE)
model.name <- do_test.filenames(files, reportnames$fn, save2file, thisdate)


##############################################
# Test that all expected files exist
##############################################
do_test.file_set(crops, irrigs, rcsps, socs, sens, gcms, vars, reportnames$sim, save2file)


##############################################
# Test file contents
##############################################
do_test.files(files, reportnames$data, landseamask, save2file, thisdate)

