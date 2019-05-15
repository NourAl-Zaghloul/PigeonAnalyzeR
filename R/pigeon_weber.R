pigeon_weber <- function(){}


#### Original Code ----
# #### Introductions
# # This script does the actual analysis of the clean data:
# # Weber scores (translated to r from Price et al. 2004 formulae)
# # As closely emulating
#
# # packages:
# library(tidyverse) # Shouldn't need to load because source(0_...) would load it
# library(pracma) #for the equivalent to the matlab erfc and lsqnonlin functions
#
# # This script assumes that you'll receive long data of a 2AFC study (one line per answer)
# # "Ratio","participant", and "correct" need to be column names
# # Ratio = number between 0 and 1 that is the ratio of the numbers displayed
# # participant = participantID
# # correct = 1 or 0, if they got the trial right or not
#
# # Data = DotData & NaturalData
#
# #### Creating Data for weber
#
# # Getting all unique participant IDs
# participants <- unique(c(DotData$participant, NaturalData$participant))
#
# # Collapses data by ratio and participant
# #. Calculate mean accuracy at a ratio & frequency ratio occured
# D_ratioXaccuracy_participant <- DotData %>%
#   group_by(Ratio, participant) %>%
#   summarise(accuracy = mean(correct),
#             freq     = n()) %>%
#   mutate(arrayType = "Dot")
#
# N_ratioXaccuracy_participant <- NaturalData %>%
#   group_by(Ratio, participant) %>%
#   summarise(accuracy = mean(correct),
#             freq     = n()) %>%
#   mutate(arrayType = "Natural")
#
# # adding in anchorpoints for each participant
# D_anchor <- tibble(
#   participant = participants,
#   Ratio = 1,
#   freq = 1,
#   accuracy = .5,
#   arrayType = 'Dot')
# D_ratioXaccuracy_participant <- bind_rows(D_ratioXaccuracy_participant, D_anchor)
#
# N_anchor <- tibble(
#   participant = participants,
#   Ratio = 1,
#   freq = 1,
#   accuracy = .5,
#   arrayType = 'Natural')
# N_ratioXaccuracy_participant <- bind_rows(N_ratioXaccuracy_participant, N_anchor)
#
# #### Creating necessary functions
#
# # remaking the erfc function to replicate the matlab one
# # shouldn't make a difference b/w this and pracma::erfc but just in case...
# erfc2 <- function(x){
#   OUT <- 2*pnorm(x*sqrt(2), lower.tail = FALSE)
#   return(OUT)
# }
#
# # Creating the ANSmodel function
# ANSModel <- function(weber){
#   OUT <- rep(0, length(ratio))
#   for(j in seq(length(ratio))){
#     OUT[j] <-
#       1 - (0.5 * erfc2(abs(ratio[j]-1)/( sqrt(2)*weber*sqrt(ratio[j]^2 + 1))))
#   }
#   return(OUT)
# }
#
# # Creating the lsqWeber function
# lsqWeber <- function(weber){
#   modelY <- ANSModel(weber)
#   y <- subAcc - modelY
#   return(y)
# }
#
# #### w for each participant
#
# # creates weber fraction output & cor with model ("Results" + "pearR" in matlab)
# D_w <- tibble(
#   participants = participants,
#   results = rep(0, length(participants)),
#   correlation = rep(0,length(participants))
# )
# N_w <- tibble(
#   participants = participants,
#   results = rep(0, length(participants)),
#   correlation = rep(0,length(participants))
# )
#
# # creates an overview of the processed data ("AccArray" in matlab)
# D_overview <- tibble(
#   Participant = character(),
#   Ratio = double(),
#   SubjectAccuracy = double(),
#   W = double(),
#   ModelAccuracy = double(),
#   ssq = double(),
#   Residual = double()
# )
# N_overview <- tibble(
#   Participant = character(),
#   Ratio = double(),
#   SubjectAccuracy = double(),
#   W = double(),
#   ModelAccuracy = double(),
#   ssq = double(),
#   Residual = double()
# )
#
#
# # Getting the w score for each participant
# for(i in 1:length(participants)){
#
#   # Taking only a single participant's data
#   D_participant <- D_ratioXaccuracy_participant[
#     D_ratioXaccuracy_participant$participant == participants[i],]
#   N_participant <- N_ratioXaccuracy_participant[
#     N_ratioXaccuracy_participant$participant == participants[i],]
#
#   # creating vectors for Ratio & Accuracy
#   ratio <- D_participant$Ratio
#   subAcc <- D_participant$accuracy
#
#   # perfoming least squares non-linear regression
#   #. tolx change to match matlab default (though doesn't matter for this granularity)
#   D_results <- lsqnonlin(lsqWeber,.25, options = list(tolx = 1e-06))
#   # creating what we would predict if model was 100% accurate
#   D_modelAccuracy <- ANSModel(D_results[[1]])
#   # creates dataframe which lists the participant, their w, and correlation between
#   #. their w and the model's prediction
#   D_w[i,2] <- D_results[[1]]
#   D_w[i,3] <- cor(subAcc, D_modelAccuracy)
#   # Larger dataframe showing the nuances of their results
#   #. Missing Residuals (function doesn't give them)
#   # TODO: Figure out how to get residuals out of lsqnonlin()
#   D_overviewtemp <- tibble(
#     Participant = rep(participants[i], length(ratio)),
#     Ratio = ratio,
#     SubjectAccuracy = subAcc,
#     W = rep(D_results[[1]],length(ratio)),
#     ModelAccuracy = D_modelAccuracy,
#     ssq = D_results[[2]],
#     Residual = rep(NA_integer_,length(ratio))
#   )
#   # Attaches new data to the df with every participant
#   D_overview <- bind_rows(D_overview, D_overviewtemp)
#
#
#   # repeats the same thing as above, but for the other test type
#   ratio <- N_participant$Ratio
#   subAcc <- N_participant$accuracy
#   N_results <- lsqnonlin(lsqWeber,.1)
#   N_modelAccuracy <- ANSModel(N_results[[1]])
#   N_w[i,2] <- N_results[[1]]
#   N_w[i,3] <- cor(subAcc, N_modelAccuracy)
#   N_overviewtemp <- tibble(
#     Participant = rep(participants[i], length(ratio)),
#     Ratio = ratio,
#     SubjectAccuracy = subAcc,
#     W = rep(N_results[[1]],length(ratio)),
#     ModelAccuracy = N_modelAccuracy,
#     ssq = N_results[[2]],
#     Residual = rep(NA_integer_,length(ratio))
#   )
#   N_overview <- bind_rows(N_overview, N_overviewtemp)
# }

# DONE! (:

