require(ncdf4)

save2file <- TRUE
ignore <- list("years"=FALSE)

# Paths
# path_AgMIP.output <- "/project2/ggcmi/AgMIP.output/"
path_AgMIP.output <- "/Volumes/Reacher/GGCMI/runs_ggcmi_3b/tests/test_shortspinup/output-2020-06-15-202043-pp"
landseamask_file <- "/Volumes/Reacher/GGCMI/inputs/phase3/ISIMIP3/landseamask/landseamask_no_antarctica.nc"
report_dir <- "" # Set to "" to use directory above working directory (top-level directory of outputs)
report_dir_web <- "" # Set to "" to ignore
ggcmi_function_file <- "/Users/Shared/GGCMI/inputs/phase3/ISIMIP3/_MATLAB_ISIMIP3/phase3_quality_check/GGCMI_phase3_check_functions.r"

# settings and definitions ####
model.name <- "lpj-guess"
crops <- c("mai","ri1","ri2","soy","swh","wwh","mil","sor","bea")
irrigs <- c("firr","noirr")
rcsps <- c("picontrol","historical","ssp126","ssp585")
socs <- c("histsoc","2015soc")
sens <- c("default","2015co2")
gcms <- c("GFDL-ESM4","IPSL-CM6A-LR","MPI-ESM1-2-HR","MRI-ESM2-0","UKESM1-0-LL")
vars <- c("yield","biom","cnyield","plantday","plantyear","anthday","matyday","pirrww","aet","soilmoist1m",
          "transp","evap","runoff","rootm","tnrup","tnrin","tnrloss","n2oemis","n2emis","nleach","tcemis","ch4emis")


#######################
# Set up
#######################
source(ggcmi_function_file)
thisdate <- date()

# Get and change working directory,
# ensuring that the last character of path_AgMIP.output is a slash
if (substr(path_AgMIP.output, nchar(path_AgMIP.output), nchar(path_AgMIP.output)) != "/") {
  path_AgMIP.output <- paste0(path_AgMIP.output, "/")
}
working_dir <- paste0(path_AgMIP.output,model.name,"/phase3b")
if (!dir.exists(working_dir)) {
  stop(paste("working_dir does not exist:", working_dir))
}
setwd(working_dir)

# Import landseamask
landseamask <- readmask.nc(landseamask_file)

# Set up reports
reportnames <- setup_reports(report_dir, report_dir_web, save2file, thisdate, model.name)


#######################
# Test filenames
#######################
files <- dir(recursive=TRUE, include.dirs=FALSE)
do_test.filenames(files, reportnames, save2file, thisdate, model.name, ignore)


##############################################
# Test that all expected files exist
##############################################
do_test.file_set(crops, irrigs, rcsps, socs, sens, gcms, vars, reportnames, save2file, ignore)


##############################################
# Test file contents (only netCDFs)
##############################################
files <- dir(pattern = "*.nc|*.nc4", recursive=TRUE, include.dirs=FALSE)
do_test.files(files, reportnames, landseamask, save2file, thisdate)

