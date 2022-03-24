#' Set console_data used to build the command line
#'
#' @param run2simulate Run name to simulate
#' @param project_path Path to the directory of input files
#'
#' @return a list containing :
#' `run_names` list of the runs to simulate
#' `sit2simulate`list of situations to simulate
#' `Var` list of items for varietal parameters
#' `NonVar` list of items for non-varietal parameters
#' `Man` list of items for farming practices parameters
#' `Soil` list of items for soil parameters
#' `Site` list of items for site parameters
#'
#'
#'
#'
set_console_data <- function(run2simulate,project_path){

  runs_list <- set_runs_list(run2simulate,project_path)

  console_data = list()
  sit2simulate = list()
  run_names    = list()
  Var          = list()
  NonVar       = list()
  Man          = list()
  Soil         = list()
  Site         = list()
  i=1
  while(i < length(runs_list)){
    run_names[[length(run_names)+1]] = runs_list[[i]]
    items_data = runs_list[[i+1]]
    for(k in 1:length(items_data$situation) ){
      if(! is.element(items_data$Var[[k]],Var)){
        Var[[length(Var)+1]]       <- items_data$Var[[k]]
      }
      if(! is.element(items_data$NonVar[[k]],NonVar)){
        NonVar[[length(NonVar)+1]] <- items_data$NonVar[[k]]
      }
      if(! is.element(items_data$Man[[k]],Man)){
        Man[[length(Man)+1]]       <- items_data$Man[[k]]
      }
      if(! is.element(items_data$Soil[[k]],Soil)){
        Soil[[length(Soil)+1]]     <- items_data$Soil[[k]]
      }
      if(! is.element(items_data$Site[[k]],Site)){
        Site[[length(Site)+1]]     <- items_data$Site[[k]]
      }


      sit2simulate[[length(sit2simulate)+1]]   <- items_data$situation[[k]]
    }


    i= i+2
  }

  console_data$sit2simulate = sit2simulate
  console_data$run_names    = run_names
  console_data$Var          = Var
  console_data$NonVar       = NonVar
  console_data$Man          = Man
  console_data$Soil         = Soil
  console_data$Site         = Site

  return(console_data)

}
