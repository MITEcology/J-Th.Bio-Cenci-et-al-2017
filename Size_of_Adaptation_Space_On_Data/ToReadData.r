library(MASS)

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
create_list <- function(M, file_names, treatment, both = TRUE){
  # Here we just extract the adjacent matrices from the data and I put all of them into a list
  N <- list()
  element <- 1
  if (both == TRUE){
    anno <- c(2009, 2010)
  } else if (both == 2009){
    anno = c(2009);
  } else {
    anno <- c(2010);
  }
  for (h in 1:length(anno)){
    for (k in 1:length(file_names)){
      SM <- M[M$Treatment == treatment,];
      Site <- SM[SM$Site2 == file_names[k],];
      Site <- Site[Site$Year == anno[h],];
      Site$Plant_sp <- as.vector(Site$Plant_sp)
      Site$Insect_sp <- as.vector(Site$Insect_sp)
      N1_plants <- unique(Site$Plant_sp)
      N1_animals <- unique(Site$Insect_sp)
      
      N[[element]] <- matrix(0,nrow=length(N1_plants),ncol=length(N1_animals))
      
      rownames(N[[element]]) <- N1_plants
      colnames(N[[element]]) <- N1_animals
      
      for (i in 1:length(Site$Plant_sp)){
        N[[element]][Site$Plant_sp[i],Site$Insect_sp[i]]  <- 1
      }
      element <- element + 1;
    }
  }
  return(N)
}