require(ncdf4)

save2file <- FALSE
ignore <- list("years"=FALSE)

# Paths
path_AgMIP.output <- "/p/projects/macmit/data/GGCMI/AgMIP.output/"
#path_AgMIP.output <- "/Volumes/Reacher/GGCMI/runs_ggcmi_3b/tests/test_shortspinup/output-2020-06-15-202043-pp"
landseamask_file <- "/p/projects/macmit/data/GGCMI/AgMIP.input/phase3/landseamask/seamask.nc" #"/Volumes/Reacher/GGCMI/inputs/phase3/ISIMIP3/landseamask/landseamask_no_antarctica.nc"
cropcalendar_dir <- "/p/projects/macmit/data/GGCMI/AgMIP.input/phase3/crop_calendar/"
report_dir <- "" # Set to "" to use directory above working directory (top-level directory of outputs)
report_dir_web <- "" # Set to "" to ignore
ggcmi_function_file <- "/home/cmueller/github_ggcmi/GGCMI_phase3_check_functions.r"

# settings and definitions ####
model.name <- "ORCHIDEE-crop"
crops <- c("mai","ri1","ri2","soy","swh","mil","sor","wwh","bea") #"sgb","pot","rap","bar")
irrigs <- c("firr","noirr")
rcsps <- c("picontrol","historical","ssp126","ssp585","ssp370")
socs <- c("histsoc","2015soc")
sens <- c("default","2015co2","1850co2")
gcms <- c("GFDL-ESM4","IPSL-CM6A-LR","MPI-ESM1-2-HR","MRI-ESM2-0","UKESM1-0-LL")
vars <- c("yield","biom","cnyield","plantday","plantyear","harvyear","anthday","matyday","pirnreq","aet","soilmoist1m","soilmoistmat",
          "transp","evap","soilevap","runoff","rootm","tnrup","tnrin","tnrloss","n2oemis","n2emis","nleach","tcemis","ch4emis","maturitystatus",
          "maturityindex")
#phase <- "phase3b"
phase <- "phase3a_attrici"
if (phase == "phase3a_attrici") {
  gcms <- c("gswp3-w5e5", "20crv3-w5e5")
  socs <- c("histsoc-a0", "histsoc-a1")
  sens <- c("default", "1901co2")
  rcsps <- c("obsclim", "counterclim")
}



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
working_dir <- paste0(path_AgMIP.output,model.name,"/",phase)
if (!dir.exists(working_dir)) {
  stop(paste("working_dir does not exist:", working_dir))
}
setwd(working_dir)

# Import landseamask
landseamask <- readmask.nc(landseamask_file)

# Set up reports
reportnames <- setup_reports(report_dir, report_dir_web, save2file, thisdate, model.name, phase)


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

