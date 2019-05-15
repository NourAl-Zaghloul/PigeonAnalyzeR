#' Quickly takes raw data to processed data
#'
#' @param method vector of types of data being used
#' @param patterns.regex vector of Regular Expression to find files
#' @param patterns.exc vector of Regular Expression to exclude from files
#' @param path vector of file locations
#' @return A dataframe with aggregated looking, widened data, and trial information
#' @example
#' pigeon_quick(c("datavyu", "habit"), c("NumbR10", "Number Replication"), c("Number Replication", ".txt"))
pigeon_quick <- function(method, patterns.regex = c(rep(NULL,length(method))), patterns.exc = c(rep(NULL,length(method))), path = c(rep(getwd(),length(method))), coder = NULL){
  ##$ Basically just runs through the pimport, pclean, and pprocess with defaults

  ##$ Sets parameters for doing reliability
  if (method == "reliability"){
    method <- c("datavyu","datavyu2")
    reliability <- TRUE
  }

  Inprogress <- list(rep("",length(method)))

  ##. Completely does one element before moving onto the next
  for(i in seq(method)){
    Inprogress[[i]] <- pigeon_import(method[i], patterns.regex[i], patterns.exc[i], path[i])
    Inprogress[[i]] <- pigeon_clean(Inprogress[[i]], method[i])
  }

  ##. Finishes the pprocess step and accounts for reliability
  if (reliability){
    OUT <- pigeon_process(x = Inprogress, method = "reliability", coder = coder)
  } else{
    OUT <- pigeon_process(x = Inprogress, method)
  }

  invisible(return(OUT))

}
