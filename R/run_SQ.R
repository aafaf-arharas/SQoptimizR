#' Build and call the command line
#'
#'
#' @param param_values A named vector that contains the value(s) and name(s)
#' of the parameters to force for each situation to simulate.
#' @param model_options Options for the model wrapper
#' @param sim_output_path Path to where we save the simulations
#' @param situation Situation to simulate
#'
#' @importFrom tools file_path_as_absolute

run_SQ <- function(param_values, model_options, sim_output_path, situation) {

  home.wd             <- model_options$home.wd
  console_path        <- model_options$console_path
  projects_path       <- file_path_as_absolute(model_options$projects_path)
  print_console       <- model_options$print_console
  saves_daily_output  <- model_options$saves_daily_output
  console_data        <- model_options$console_data
  Param_list          <- model_options$Param_list


  ## Param_list and param_values must have the same length

  index = c()
  for(i in 1:length(Param_list$name)){
    if(!is.element(Param_list$name[[i]],names(param_values))){
      print(Param_list$name[[i]])
      index = c(index,i)
    }
  }

  v <- seq(1, length(Param_list$name))
  index1 = c()
  for(i in v){
    if(!is.element(i, index)){
      index1 = c(index1,i)
    }
  }

  Param_list <- Param_list[index1,]



  #Select only items for which types are in Param_list
  db = which(duplicated (Param_list$type))
  type = Param_list$type[-db]
  print(db)
  if( ! is.element(TRUE,duplicated (Param_list$type))){
    type = Param_list$type
  }
  print(type)

  #Build the command line
  console_text = ""

  #Add situation to simulate
  console_text = paste(console_text,"--iRun",sep = " ")
  console_text = paste(console_text,situation,sep = " ")


  # Add run to simulate
  console_text = paste(console_text,"--Run",sep = " ")
  for(run in console_data$run_names){
    console_text = paste(console_text,run,sep = " ")
  }


  for(t in type){
    TYPE = paste("--",t,sep="")
    console_text = paste(console_text,TYPE,sep = " ")

    ID=which(names(console_data)==t)
    items = console_data[[ID]]

    for(i in 1: length(items)){
      if(!is.na(items[[i]]) ){
        # Add item
        item = paste("[",items[[i]], "]", sep="")
        console_text = paste(console_text,item,sep = " ")
        # Add param_values
        Param_name = Param_list$name[Param_list$type == t]
        Param_val  = param_values[Param_list$type == t]
        N_TYPE= length(Param_name)
        for (n in 1:N_TYPE){console_text = paste(console_text, Param_name[n], Param_val[n], sep = " ")}
      }

    }


  }

  # Add output path
  console_text = paste(console_text,"--OutPath",sep = " ")
  console_text = paste(console_text,sim_output_path,sep = " ")


  # Call the command line
  system2(console_path,
          paste("-simoverride",
                print_console,
                saves_daily_output,
                projects_path,
                console_text,
                sep=" "),invisible=F)

}
