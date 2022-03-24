#' Set the data frame which associate each parameter to its type extracted from the project files
#'
#' @param param_info Information on the parameters to estimate.
#' #' Either
#' a list containing:
#'    - (named) vectors of upper and lower bounds (`ub` and `lb`) (-Inf and Inf can be used),
#'    - `init_values`, A data.frame containing initial
#' values to test for the parameters (optional, if not provided, or if less values
#' than number of repetitions of the minimization are provided, the, or part
#' of the, initial values will be randomly generated using LHS sampling within
#' parameter bounds).
#'
#' or
#' a named list containing for each parameter the list of situations per group
#' (`sit_list`), the vector of upper and lower bounds (one value per group)
#' (`ub` and `lb`) and the list of initial values per group
#' `init_values` (data.frame, one column per group, optional).
#' (see [here](https://sticsrpacks.github.io/CroptimizR/articles/Parameter_estimation_Specific_and_Varietal.html)
#' for an example)
#'
#' @param project_path Path to the project directory of input files
#'
#' @return A data frame containing
#' two columns :  `name` with the names of parameters and `type` with the types of each parameter
#' either Var, NonVar, Soil, Site or Man
#'
#' @importFrom stringr str_split
#' @importFrom XML xmlParse
#' @importFrom XML xmlToList
#'
#'
set_param_list <- function(param_info,project_path){

  param_names <- get_param_names(param_info)

  Management = Sys.glob(paste0(project_path,"/*.sqman"))
  Non_VarPar = Sys.glob(paste0(project_path,"/*.sqpar"))
  VarPar     = Sys.glob(paste0(project_path,"/*.sqvar"))
  site       = Sys.glob(paste0(project_path,"/*.sqsit"))
  soil       = Sys.glob(paste0(project_path,"/*.sqsoi"))

  #Initialise param_list :
  param_list = data.frame(matrix(NA,ncol=2,nrow=length(param_names)))
  names(param_list) = c("name","type")
  param_list$name = param_names

  #Extract names parameters from the project files  :
  #Man :
  data <- xmlParse(Management)
  xml_data <- xmlToList(data)
  Man <- str_split(names(xml_data$ItemsArray$ManagementItem), " ") #convertir en une liste

  #NonVar :
  data <- xmlParse(Non_VarPar)
  xml_data <- xmlToList(data)
  NonVar = list()
  for(i in 1 : length(xml_data$ItemsArray$CropParameterItem$ParamValue)){
    NonVar[length(NonVar)+1]= xml_data$ItemsArray$CropParameterItem$ParamValue[i]$Item$Key$string
  }


  #Var :
  data <- xmlParse(VarPar)
  xml_data <- xmlToList(data)
  Var = list()
  for(i in 1 : length(xml_data$ItemsArray$CropParameterItem$ParamValue)){
    Var[length(Var)+1]= xml_data$ItemsArray$CropParameterItem$ParamValue[i]$Item$Key$string
  }


  #Site :
  data <- xmlParse(site)
  xml_data <- xmlToList(data)
  Site <- str_split(names(xml_data$ItemsArray$SiteItem), " ")


  #Soil :
  data <- xmlParse(soil)
  xml_data <- xmlToList(data)
  Soil <- str_split(names(xml_data$ItemsArray$SoilItem), " ")


  #Associate the types to the parameters names :
  for(name in param_names){
    if(is.element(name,Man)){
      param_list$type[which(param_list$name==name)] = "Man"
    }
    else if(is.element(name,NonVar)){
      param_list$type[which(param_list$name==name)] = "NonVar"
    }
    else if(is.element(name,Var)){
      param_list$type[which(param_list$name==name)] = "Var"
    }
    else if(is.element(name,Site)){
      param_list$type[which(param_list$name==name)] = "Site"
    }
    else if(is.element(name,Soil)){
      param_list$type[which(param_list$name==name)] = "Soil"
    }
    else{
      print("Incorrect parameter name")
    }
  }

  return(param_list)

}
