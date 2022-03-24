#' Read the simulations (output files of SQ)
#'
#' @param sim_output_path Path to the output file
#' @param model_options Options for the model wrapper
#'
#' @return  A named list (names = situations names )
#' Each element of the list should contains a data.frame with the results obtained for all simulated variables and dates for the given situation.
#' The data.frames must have one column called `Date` containing the simulations dates, in Date or POSIXct format
#' @importFrom stringr str_split
#' @importFrom utils read.table
#'
sim_read <- function(sim_output_path, model_options) {

  Var_simule <- model_options$Var_simule
  Var_names  <- model_options$Var_names

  Sim_NEW <- {}

  ###read files of summary outputs :
  Sim_sum <- Sys.glob(paste0(sim_output_path,"/*.sqbrs"))
  sumfile <- readLines(Sim_sum)

  k=1
  while(k < length(sumfile )){
    ligne = str_split(sumfile[k],"\t")
    if(ligne[[1]]=="RUID"){
      ruid = k
      break
    }
    k=k+1
  }

  name_sit <- read.table(Sim_sum, skip=ruid, header=F, stringsAsFactors=F, sep="\t")[[1]]

  ##read files of daily outputs :
  Sim_dyn  <- readLines(paste0(sim_output_path,'/',name_sit,".sqsro"))
  i=1
  while(i < length(Sim_dyn)){
    ligne = str_split(Sim_dyn[i],"\t")
    if(ligne[[1]]=="DATE"){
      toskip = i
      break
    }
    i=i+1
  }

  Sim_dyn  <- read.table(paste0(sim_output_path,'/',name_sit,".sqsro"), dec = ",",
                         skip=toskip-1, header=T, stringsAsFactors=F, sep="\t")


  ## Fill in the Data
  N_col   <- length(names(Sim_dyn))
  N_row   <- length(Sim_dyn$DATE)
  Data    <- data.frame(matrix(NA,ncol=N_col,nrow=N_row))
  # Can't use $  so we will use the index
  L = c()  # list of index of variables to simulate in Sim_dyn
  for (v in Var_simule) {
    L[length(L)+ 1] = which(names(Sim_dyn)==v)
  }


  Data[[1]] = as.Date(Sim_dyn[[1]])
  for(i in L){
    if(i!=1){
    Data[[i]] = as.numeric(Sim_dyn[[i]])
    }
  }


  ##remove columns witn NA  values
  Drop = c()
  for (i in 1:length(Data)){
    if(!is.element(i,L)){
      Drop[length(Drop)+1]=i
    }
  }
  #print(Drop)
  if(!is.null(Drop)){
    Data = subset(Data, select = -Drop )
  }

  names(Data) = Var_names
  Sim_NEW[[name_sit]] <- Data  #Convert Data to a list (Sim_NEW)
  return(Sim_NEW)
}
