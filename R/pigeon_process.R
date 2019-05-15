#' Processes and combines cleaned data
#'
#' @param x A list of data to be processed and cleaned
#' @param method A vector listing types of input data
#' @param exports Logical or 1:3, determines what csv.s are exported and when
#' @param endformat "wide" or "long"
#' @param join "full","left","right","" determines how data will be combined
#' @return A dataframe with aggregated looking, widened data, and trial information
#' @example
#' pigeon_process(list(vyu.data, habit.data), c("datavyu", "habit"), join = "left")
pigeon_process <- function(x, method = "default", endformat = "wide", join = "inner", coder = NULL){
  ##!function: This is one of the messier and more specific functions I made.
  ###! Find a way to generalize it if possible.

  ##$ Creates a list the size of method amounts in order to work in a loop
  OUT_temp <- list(rep("",length(method)))

  ##$ populates above list with aggregated data from above
  for (i in seq(x)){
    if(method == "reliability"){
      ##$ reliability will run pprocess assuming it's datavyu and datavyu 2

      if("coder1.code01" %in% colnames(x[[1]])){
        reliability <- pigeon_process(x, method = c("datavyu","datavyu2"), endformat = "long")
      } else {
        reliability <- pigeon_process(x, method = c("datavyu2","datavyu"), endformat = "long")
      }

      reliability[is.na(reliability)] <- 0
      reliability$diff <- abs(((reliability$a.x-reliability$a.y)+(reliability$d.x-reliability$d.y))/2)

      ##$ Determines who the main coder is if not specified
      nms <- c(reliability$coder1.code01, reliability$coder2.code01)
      if (is.null(coder)){
        coder <- names(which(table(nms) == max(table(nms))))
      } else if (coder == "matrix"){
      }

      ##. Removes the main coder from the rest of the coders
      nms <- unique(nms)
      nms <- nms[!nms %in% coder]

      ##. co = correlation
      ##. mn = mean difference
      ##. tr = total number of trials
      nms_col <- paste(rep(nms, each = 3), c("co", "mn", "tr"),sep=".")

      ##$ Creates the bones of the correlation matrix
      OUT <- data.frame(matrix(NA, nrow = length(coder), ncol = length(nms_col)))
      colnames(OUT) <- nms_col
      rownames(OUT) <- coder

      ##$ Creates the correlation matrix
      for(j in seq(nms)){
        looktemp <- dplyr::filter(reliability, coder1.code01 == coder & coder2.code01 == nms[j] |
                                    coder1.code01 == nms[j] & coder2.code01 == coder)
        OUT[1, (j*3)-2] <- cor(c(looktemp$a.x,looktemp$d.x), c(looktemp$a.y,looktemp$d.y))
        OUT[1, (j*3)-1] <- mean(looktemp$diff)
        OUT[1, (j*3)-0] <- nrow(looktemp)

      }

      invisible(return(OUT))

    } else if(method[i] == "datavyu"){

      datavyu <- x[[i]]

      datavyu <- datavyu[ , c("part", "trial", "coder1.code01", "look_1.code01", "look_1.duration")]

      datavyu <- dplyr::summarise(
        dplyr::group_by(datavyu, part, trial, coder1.code01, look_1.code01),
        aggregate = sum(look_1.duration))

      datavyu$study <- gsub("[[:digit:]]", "", datavyu$part)
      datavyu$part <- gsub("[^[:digit:]]", "", datavyu$part)
      datavyu <- transform(datavyu, part      = as.numeric(part),
                           trial     = as.numeric(trial),
                           aggregate = as.numeric(aggregate),
                           study     = as.character(study))

      datavyu <- tidyr::spread(datavyu, look_1.code01, aggregate)

      colnames(datavyu) <- tolower(colnames(datavyu))

      OUT_temp[[i]] <- datavyu

    } else if (method[i] == "habit") {

      habit <- x[[i]]
      habit$part <- habit$SubjectID
      habit <- habit[ , -grep("SubjectID", names(habit))]
      habit$study <- gsub("[[:digit:]]", "", habit$part)
      habit$part <- gsub("[^[:digit:]]", "", habit$part)
      habit$part <- as.numeric(habit$part)
      colnames(habit) <- tolower(colnames(habit))

      OUT_temp[[i]] <- habit


    } else if (method[i] == "director"){

      director <- x[[i]]
      director$part <- as.numeric(director$part)
      director$trial <- as.numeric(director$trial)

      colnames(director) <- tolower(colnames(director))

      OUT_temp[[i]] <- director

    } else if (method[i] == "datavyu2"){

      datavyu2 <- x[[i]]

      datavyu2 <- datavyu2[ , c("part", "trial", "coder2.code01", "look_2.code01", "look_2.duration")]

      datavyu2 <- dplyr::summarise(
        dplyr::group_by(datavyu2, part, trial, coder2.code01, look_2.code01),
        aggregate.2 = sum(look_2.duration))

      datavyu2$study <- gsub("[[:digit:]]", "", datavyu2$part)
      datavyu2$part <- gsub("[^[:digit:]]", "", datavyu2$part)
      datavyu2 <- transform(datavyu2, part      = as.numeric(part),
                            trial     = as.numeric(trial),
                            aggregate.2 = as.numeric(aggregate.2),
                            study     = as.character(study))

      datavyu2 <- tidyr::spread(datavyu2, look_2.code01, aggregate.2)

      colnames(datavyu2) <- tolower(colnames(datavyu2))

      OUT_temp[[i]] <- datavyu2

    }

  }

  ##$ Decides how you want to join the list items
  if (join == "full"){
    OUT <- dplyr::full_join(OUT_temp[[1]],OUT_temp[[2]], by = c("part", "study","trial"))
  } else if (join == "left"){
    OUT <- dplyr::left_join(OUT_temp[[1]],OUT_temp[[2]], by = c("part", "study","trial"))
  } else if (join == "right"){
    OUT <- dplyr::right_join(OUT_temp[[1]],OUT_temp[[2]], by = c("part", "study","trial"))
  } else if (join == "semi"){
    OUT <- dplyr::semi_join(OUT_temp[[1]],OUT_temp[[2]], by = c("part", "study","trial"))
  } else if (join == "inner"){
    OUT <- dplyr::inner_join(OUT_temp[[1]],OUT_temp[[2]], by = c("part", "study","trial"))
  }

  ##$ Determines how you want the end format to be organised
  if (endformat == "wide"){
    if (any(grepl("director", method))){
      ##. Director has extra variables
      OUT_wide <- tidyr::gather(OUT, info, val, -trial, -part, -study, -sex, -code, -agemos)
    } else {
      OUT_wide <- tidyr::gather(OUT, info, val, -trial, -part, -study, -order)
    }
    OUT_wide <- tidyr::unite(OUT, info1, info, trial)
    OUT_wide <- tidyr::spread(OUT, info1, val)
  }

  invisible(return(OUT))

}

