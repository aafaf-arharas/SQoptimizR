#' Load observations
#'
#' @param obs_daily_path Path to the file of daily observations
#' @param obs_sum_path Path to the file of summary observations
#' @param project_path Path to the directory of input files
#' @param dailyVar Vector of the names of variables to estimate (variables from daily file)
#' @param sumVar  Vector of the names of variables to estimate (variables from summary file)
#'
#' @return  List of observed values to use for parameter estimation
#' A `named list` (names = situations names) of data.frame containing
#' one column named Date with the dates (Date or POSIXct format) of the different observations
#' and one column per observed variables with either the measured values or NA, if
#' the variable is not observed at the given date
#'
#' @importFrom stringr str_split
#' @importFrom utils read.csv
#' @importFrom openxlsx read.xlsx convertToDate
#'
load_obs <- function( obs_daily_path, obs_sum_path, project_path, dailyVar, sumVar) {


  # find number of rows to skip (obs) :
  obsfile <- readLines(obs_daily_path)

  k=1
  while(k < length(obsfile )){
    ligne = str_split(obsfile[k],"\t")
    if(ligne[[1]]=="ID"){
      Skip = k
      break
    }
    k=k+1
  }



  obs_daily            <- read.csv(obs_daily_path,skip=Skip-1, header=T,
                                   na.strings = "-999", sep = "\t")

  obs_sum              <- read.csv(obs_sum_path,skip=Skip-1, header=T,
                                   na.strings = "-999", sep = "\t")



  # Extract Planting dates from management file
  Management = Sys.glob(paste0(project_path,"/*.sqman"))
  data <- xmlParse(Management)
  xml_data <- xmlToList(data)
  obs_planting = list()
  names=c()

  for(i in 1: length(xml_data$ItemsArray)){
    name = xml_data$ItemsArray[i]$ManagementItem$.attrs
    names[[length(names)+1]] = substr(name, 1, nchar(name)-2)
    obs_planting[[length(obs_planting)+1]] = substr(xml_data$ItemsArray[i]$ManagementItem$SowingDate,1,10)

  }

  names(obs_planting)=names
  doublons <- which(duplicated(obs_planting))
  obs_planting <-obs_planting[-doublons]



  situations <- obs_daily$ID
  experiments <- obs_daily$EID
  N_sit = length(situations)



  obs_list       <- {}
  i=1
  while (i<N_sit & situations[i]!="" )
  {

    name_sit        <- as.character(situations[i])
    name_exp        <- as.character(experiments[i])

    Data_daily      <- obs_daily[obs_daily$ID == name_sit,c('EID','date',dailyVar)]

    Data_sum        <- obs_sum[obs_sum$ID == name_sit,c('EID', sumVar)]

    planting_Date   <- as.Date(obs_planting[names(obs_planting)==name_exp][[1]])


    Data            <- data.frame(matrix(NA,ncol=length(c("Date",dailyVar, sumVar)),nrow=length(Data_daily$date)))
    names(Data) = c("Date",dailyVar, sumVar)
    Data["Date"]  = as.Date(Data_daily$date)


    for(var in dailyVar){
      Data[var] = Data_daily[var]
    }

    for(var in sumVar){
      if(is.character(Data_sum[var][[1]])){
        Var = NA
        if(Data_sum[var] != ""){
          Var = abs(as.integer(difftime(planting_Date, Data_sum[var][[1]], units = "days")))
        }

        Data[var] = Var
      }else{
        Data[var] =  Data_sum[var]
      }
    }


    obs_list[[name_sit]]    <- Data
    i=i+1
  }

  return(obs_list)

}








