#' Set the list of items associated to a given run name (situations,Var,NonVar,Site,Soil,Man)
#'
#' @inheritParams set_console_data
#'
#' @return A `named list` (names = runs names) of data frame containing the columns :
#' `situation` the associated situations
#' `Var`  items for varietal parameters
#' `NonVar` items for non-varietal parameters
#' `Soil` items for soil parameters
#' `Site` items for site parameters
#' `Man` items for managnement parameters
#' @importFrom XML xmlParse
#' @importFrom XML xmlToList
#'
set_runs_list <- function(run2simulate,project_path) {

    runs_path <- Sys.glob(paste0(project_path,"/*.sqrun"))

    #Create list of run_name and the dataframe associated
    runs_list = list()

    #read the runs file :
    data <- xmlParse(runs_path)
    xml_data <- xmlToList(data)

    #number of run names
    N=length(xml_data$ItemsArray)
    print(N)



    for( n in 1:N){
      #Add run_name
      run_name = xml_data$ItemsArray[n]$RunItem$.attrs

      if(!is.null(run2simulate)){
        if(is.element(run_name,run2simulate)){
          runs_list[[length(runs_list)+1]] = run_name

          #Create items_data
          Nrow = length(xml_data$ItemsArray[n]$RunItem$Multi$MultiRunsArray)
          items_data <- data.frame(matrix(NA,ncol=6,nrow=Nrow))
          names(items_data) = c("situation","Soil","Var","NonVar", "Man","Site")
          #Fill in items_data
          for(i in 1:Nrow){

            items_data[i,]$situation <- xml_data$ItemsArray[n]$RunItem$Multi$MultiRunsArray[i]$MultiRunItem$RunIDAgMip
            items_data[i,]$Man       <- xml_data$ItemsArray[n]$RunItem$Multi$MultiRunsArray[i]$MultiRunItem$ManagementItem
            items_data[i,]$NonVar    <- xml_data$ItemsArray[n]$RunItem$Multi$MultiRunsArray[i]$MultiRunItem$ParameterItem
            items_data[i,]$Site      <- xml_data$ItemsArray[n]$RunItem$Multi$MultiRunsArray[i]$MultiRunItem$SiteItem
            items_data[i,]$Soil      <- xml_data$ItemsArray[n]$RunItem$Multi$MultiRunsArray[i]$MultiRunItem$SoilItem
            items_data[i,]$Var       <- xml_data$ItemsArray[n]$RunItem$Multi$MultiRunsArray[i]$MultiRunItem$VarietyItem
          }


          runs_list[[length(runs_list)+1]] = items_data
        }
      }


    }


    return(runs_list)

}
