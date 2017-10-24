library(MASS)
source('ReadList.r')

TakeMatrixFromData <- function(File_Name_){
  M <- read.csv(File_Name_,header = TRUE, sep = ';')
  M <- M[M$Freq>0,]
  M$Plant_sp <- as.vector(M$Plant_sp)
  M$Insect_sp <- as.vector(M$Insect_sp)

  ##### META  WEB #####
  all_plants <- unique(M$Plant_sp)
  all_animals <- unique(M$Insect_sp)
  all_N <- matrix(0,nrow=length(all_plants),ncol=length(all_animals))
  rownames(all_N) <- all_plants
  colnames(all_N) <- all_animals

  # To make it binary
  for (i in 1:length(M$Plant_sp)){
    all_N[M$Plant_sp[i],M$Insect_sp[i]]  <- 1
  }
  #########   Adjacent Matrices  ###########
  nomi_plots <- c('Albufera', 'Llimpa', 'Tirant');
  treatment <- 'Invaded'
  beta <- create_list(M, nomi_plots, treatment)
  treatment <- 'Removed'
  betaR <- create_list(M, nomi_plots, treatment)

  return(list(beta = beta, betaR = betaR))
}