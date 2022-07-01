break.down.anchovy <- function(cluster,
                               logistic.parameters.anchovy = c(-20.49893, 0.2029001),
                               diagnostic.plots = T) {
  cluster.age.0 <- cluster.age.1 <- cluster # creates two copies of cluster

  names(cluster)
  cluster$meanwg.anch.age.0 <- NA
  cluster$num.anch.age.0 <- NA
  cluster$sigmaindiv.anch.age.0 <- NA

  cluster$meanwg.anch.age.1 <- NA
  cluster$num.anch.age.1 <- NA
  cluster$sigmaindiv.anch.age.1 <- NA

  cluster.age.0$meanwg.anch.age.0 <- NA
  cluster.age.0$num.anch.age.0 <- NA
  cluster.age.0$sigmaindiv.anch.age.0 <- NA

  cluster.age.0$meanwg.anch.age.1 <- NA
  cluster.age.0$num.anch.age.1 <- NA
  cluster.age.0$sigmaindiv.anch.age.1 <- NA

  cluster.age.1$meanwg.anch.age.0 <- NA
  cluster.age.1$num.anch.age.0 <- NA
  cluster.age.1$sigmaindiv.anch.age.0 <- NA

  cluster.age.1$meanwg.anch.age.1 <- NA
  cluster.age.1$num.anch.age.1 <- NA
  cluster.age.1$sigmaindiv.anch.age.1 <- NA

  probability.age.0 <- 1 - exp(logistic.parameters.anchovy[1] + seq(1, 20) * 10 * logistic.parameters.anchovy[2]) / (1 + exp(logistic.parameters.anchovy[1] + seq(1, 20) * 10 * logistic.parameters.anchovy[2]))

  probability.age.1 <- 1 - probability.age.0

  if (sum((probability.age.1 + probability.age.0) != 1) > 0) stop("There is something wrong with the logistic model") # it has to be all ones


  # build a matrix of probabilities

  anch.probability.age.0.matrix <- matrix(rep(probability.age.0, dim(cluster.age.0)[1]), nrow = dim(cluster.age.0)[1], byrow = T)

  anch.probability.age.1.matrix <- matrix(rep(probability.age.1, dim(cluster.age.1)[1]), nrow = dim(cluster.age.1)[1], byrow = T)

  # it has to be all 1's
  if (sum((anch.probability.age.0.matrix + anch.probability.age.1.matrix) != 1)) stop("There is something wrong with either one of the probability matrices") # it has to be all ones


  cluster.age.0[, match("L1", names(cluster.age.0)):match("L20", names(cluster.age.0))] <- cluster[, match("L1", names(cluster)):match("L20", names(cluster))] * anch.probability.age.0.matrix
  cluster.age.1[, match("L1", names(cluster.age.1)):match("L20", names(cluster.age.1))] <- cluster[, match("L1", names(cluster)):match("L20", names(cluster))] * anch.probability.age.1.matrix

  # if(diagnostic.plots == T){
  #  x11()
  # plot(as.vector(as.matrix(cluster[ , match ( "L1" , names(cluster)  ): match ( "L20" , names(cluster)  )])) , as.vector(as.matrix( cluster.age.0[ , match ( "L1" , names(cluster.age.0)  ): match ( "L20" , names(cluster.age.0)  )]  + cluster.age.1[ , match ( "L1" , names(cluster.age.1)  ): match ( "L20" , names(cluster.age.1)  )])), main = "reconstructed proportions vs original proportions") # it has to be 1 to 1
  #  abline(0,1)
  # }


  ## calculate generalized length by interval from 0.5 to 20.5 , SL  to TL: TL =   1.137 * SL  + 5.1;   weight = exp(-12.964) * (df$TL)^3.387 . this are hard coded but could be made arguments in a function
  standard.length <- seq(5, 205, l = 200000) # mm
  length.class <- rep(seq(1, 20), rep(10000, 20)) # cm
  total.length <- (1.137 * standard.length + 5.100) / 10 # cm

  generalized.length.TS <- sqrt(unlist(lapply(split(total.length^2, length.class), FUN = mean)))
  generalized.length.weight <- (unlist(lapply(split(total.length^3.387, length.class), FUN = mean)))^(1 / 3.387)


  weight.vector <- exp(-12.964) * generalized.length.weight^3.387 # kg
  TS.wg.vector <- -13.87 * log10(generalized.length.TS) - 11.797
  sigma.wg.vector <- 10^(TS.wg.vector / 10)
  TS.ind.vector <- 20 * log10(generalized.length.TS) - 68.09875
  sigma.ind.vector <- 10^(TS.ind.vector / 10)

  #
  # if(diagnostic.plots == T){
  # x11()
  # plot(1/sigma.wg.vector ,  1/sigma.ind.vector* weight.vector, main  =  "comparing biomass densities")
  #  abline(0 ,1 ) #this is correct.
  # }


  weight.matrix <- matrix(rep(weight.vector, dim(cluster.age.0)[1]), nrow = dim(cluster.age.0)[1], byrow = T)
  sigma.wg.matrix <- matrix(rep(sigma.wg.vector, dim(cluster.age.0)[1]), nrow = dim(cluster.age.0)[1], byrow = T)
  sigma.ind.matrix <- matrix(rep(sigma.ind.vector, dim(cluster.age.0)[1]), nrow = dim(cluster.age.0)[1], byrow = T)
  # compare mean weight

  new.weight <- apply(cluster[, match("L1", names(cluster)):match("L20", names(cluster))] * weight.matrix, FUN = sum, MAR = 1)

  # if(diagnostic.plots == T){
  # x11()
  # par(mfrow = c(1,2))
  # plot(cluster$meanwg.anch, new.weight,   main  = "Cluster mean weight vs reconstructed mean weigth" , ylab = "reconstructed mean weight" , xlab = "Cluster mean weight")
  # abline(0,1)
  # abline(0,1.05, lty =2)
  # abline(0,0.95, lty =2)
  # }


  new.sigma.ind <- apply(cluster[, match("L1", names(cluster)):match("L20", names(cluster))] * sigma.ind.matrix, FUN = sum, MAR = 1)

  # if(diagnostic.plots == T){
  # plot(cluster$sigmaindiv.anch[cluster$prop.anch >0], new.sigma.ind[cluster$prop.anch >0], main  = "Cluster mean sigma_bs  vs reconstructed sigma_bs", xlab = "Cluster mean sigma_bs",  ylab = "Reconstructed mean sigma_bs")
  # abline(0,1)
  # abline(0,1.05, lty =2)
  # abline(0,0.95, lty =2)
  # }




  new.sigma.wg <- apply(cluster[, match("L1", names(cluster)):match("L20", names(cluster))] * sigma.wg.matrix * weight.matrix, FUN = sum, MAR = 1) / apply(cluster[, match("L1", names(cluster)):match("L20", names(cluster))] * weight.matrix, FUN = sum, MAR = 1)

  # if(diagnostic.plots == T){
  #  plot(cluster$sigmawg.anch[cluster$prop.anch >0], new.sigma.wg[cluster$prop.anch >0]  , main  = "Cluster mean sigma_bs_wg  vs reconstructed sigma_bs_wg", xlab = "Cluster mean sigma_bs_weight",  ylab = "Reconstructed mean sigma_bs_weight")
  # abline(0,1)
  # abline(0,1.05, lty =2)
  # abline(0,0.95, lty =2)
  # }

  # if(diagnostic.plots == T){
  #  plot(1/cluster$sigmawg.anch[cluster$prop.anch >0], 1/new.sigma.wg[cluster$prop.anch >0]   , main  = "Biomass density comparison" , xlab = "Cluster biomass density",  ylab = "Reconstructed biomass density")
  # abline(0,1)
  # abline(0,1.05, lty =2)
  # abline(0,0.95, lty =2)
  # }

  #                                   if(diagnostic.plots == T){

  # plot(1/new.sigma.ind[cluster$prop.anch >0]*new.weight[cluster$prop.anch >0] ,  1/new.sigma.wg[cluster$prop.anch >0],  , main  = "Reconstructed Biomass density internal comparison",  xlab = "1 / sigma_bs",  ylab = "1 / sigma_bs * mean weight" ) # IT'S very close to 1:1 buyt is not exactly there. I have to check if using the mean point in each age class makes a difference
  # abline(0,1)
  # abline(0,1.05, lty =2)
  #                                          abline(0,0.95, lty =2)
  #                                        }



  # comparing proportions of anchovy lengths broken down by age anprop.anch.age.0 + prop.anch.age.1 = prop.anch

  if (sum(round(as.vector(as.matrix(cluster.age.0[, match("L1", names(cluster.age.0)):match("L20", names(cluster.age.0))] + cluster.age.1[, match("L1", names(cluster.age.1)):match("L20", names(cluster.age.1))])) - as.vector(as.matrix(cluster[, match("L1", names(cluster)):match("L20", names(cluster))])), 3) != 0)) stop("The sum of the proportions of age 0 and age 1+ anchovy don't match that of all  anchovy")


  # computing numbers and weights to  0 and age 1
  # harmonize age.0 and age.0

  cluster$num.anch.age.0 <- cluster.age.0$num.anch.age.0 <- cluster.age.1$num.anch.age.0 <- apply(cluster$num.anch * cluster.age.0[, match("L1", names(cluster.age.0)):match("L20", names(cluster.age.0))], FUN = sum, MAR = 1)

  cluster$num.anch.age.1 <- cluster.age.0$num.anch.age.1 <- cluster.age.1$num.anch.age.1 <- apply(cluster$num.anch * cluster.age.1[, match("L1", names(cluster.age.1)):match("L20", names(cluster.age.1))], FUN = sum, MAR = 1)

  if (sum(round(cluster.age.0$num.anch - cluster.age.0$num.anch.age.1 - cluster.age.0$num.anch.age.0, 3) > 0) > 0) stop("the number of age 0 and age 1 do not sum up to the number of total ancxhovy")



  # now I need to standardize them to sum up to 1
  prop.temporary.age.0 <- apply(as.matrix(cluster.age.0[, match("L1", names(cluster.age.0)):match("L20", names(cluster.age.0))]), FUN = sum, MAR = 1)

  prop.temporary.age.0.matrix <- matrix(prop.temporary.age.0, nrow = dim(cluster.age.0)[1], ncol = match("L20", names(cluster.age.0)) - match("L1", names(cluster.age.0)) + 1)

  cluster.age.0[, match("L1", names(cluster.age.0)):match("L20", names(cluster.age.0))] <- cluster.age.0[, match("L1", names(cluster.age.0)):match("L20", names(cluster.age.0))] / prop.temporary.age.0.matrix

  cluster.age.0[, match("L1", names(cluster.age.0)):match("L20", names(cluster.age.0))][is.na(cluster.age.0[, match("L1", names(cluster.age.0)):match("L20", names(cluster.age.0))])] <- 0 #  replace NAs

  # Check age-0 porportions = aqui!!!
  if (sum(round(apply(as.matrix(cluster.age.0[, match("L1", names(cluster.age.0)):match("L20", names(cluster.age.0))]), FUN = sum, MAR = 1), 3) != 0 & round(apply(as.matrix(cluster.age.0[, match("L1", names(cluster.age.0)):match("L20", names(cluster.age.0))]), FUN = sum, MAR = 1), 3) != 1) > 0) stop("The length proportions do not sum to 1 or 0")



  ## repeat the above step for age 1
  prop.temporary.age.1 <- apply(as.matrix(cluster.age.1[, match("L1", names(cluster.age.1)):match("L20", names(cluster.age.1))]), FUN = sum, MAR = 1)
  prop.temporary.age.1

  prop.temporary.age.1.matrix <- matrix(prop.temporary.age.1, nrow = dim(cluster.age.1)[1], ncol = match("L20", names(cluster.age.1)) - match("L1", names(cluster.age.1)) + 1)

  cluster.age.1[, match("L1", names(cluster.age.1)):match("L20", names(cluster.age.1))] <- cluster.age.1[, match("L1", names(cluster.age.1)):match("L20", names(cluster.age.1))] / prop.temporary.age.1.matrix

  cluster.age.1[, match("L1", names(cluster.age.1)):match("L20", names(cluster.age.1))][is.na(cluster.age.1[, match("L1", names(cluster.age.1)):match("L20", names(cluster.age.1))])] <- 0 #  replace NAs

  # Check age-1 porportions =
  if (sum(round(apply(as.matrix(cluster.age.1[, match("L1", names(cluster.age.1)):match("L20", names(cluster.age.1))]), FUN = sum, MAR = 1), 3) != 0 & round(apply(as.matrix(cluster.age.1[, match("L1", names(cluster.age.1)):match("L20", names(cluster.age.1))]), FUN = sum, MAR = 1), 3) != 1) > 0) stop("The length proportions do not sum to 1 or 0")


  # going to do reverse calculation

  num.anch.age.0.matrix <- matrix(cluster$num.anch.age.0, nrow = dim(cluster.age.0)[1], ncol = match("L20", names(cluster.age.0)) - match("L1", names(cluster.age.0)) + 1)


  num.anch.age.1.matrix <- matrix(cluster$num.anch.age.1, nrow = dim(cluster.age.1)[1], ncol = match("L20", names(cluster.age.1)) - match("L1", names(cluster.age.1)) + 1)


  # now I need to calculate the weight of each group
  # if(diagnostic.plots == T){

  # par(mfrow = c(1,2))

  # plot(cluster$meanwg.anch , apply( weight.matrix * as.matrix(cluster[ , match ( "L1" , names(cluster.age.0)  ): match ( "L20" , names(cluster.age.0)  )]) , FUN = sum , MAR =1))
  # abline(0,1)
  # abline(0, 1.05)
  # abline(0, 0.95)

  # }

  cluster$meanwg.anch.age.0 <- cluster.age.0$meanwg.anch.age.0 <- cluster.age.1$meanwg.anch.age.0 <- apply(weight.matrix * as.matrix(cluster.age.0[, match("L1", names(cluster.age.0)):match("L20", names(cluster.age.0))]), FUN = sum, MAR = 1)

  cluster$meanwg.anch.age.1 <- cluster.age.0$meanwg.anch.age.1 <- cluster.age.1$meanwg.anch.age.1 <- apply(weight.matrix * as.matrix(cluster.age.1[, match("L1", names(cluster.age.1)):match("L20", names(cluster.age.1))]), FUN = sum, MAR = 1)

  # verify that the weighted average of age 0 and age 1 matches to all anchovy
  names(cluster)
  if (diagnostic.plots == T) {
    par(mfrow = c(1, 2))
    plot(cluster$meanwg.anch, (cluster$meanwg.anch.age.0 * cluster$num.anch.age.0 + cluster$meanwg.anch.age.1 * cluster$num.anch.age.1) / (cluster$num.anch.age.0 + cluster$num.anch.age.1))
    abline(0, 1) # yes
    abline(0, 1.05)
    abline(0, 0.95)

    plot((cluster$meanwg.anch.age.0 * cluster$num.anch.age.0 + cluster$meanwg.anch.age.1 * cluster$num.anch.age.1) / (cluster$num.anch.age.0 + cluster$num.anch.age.1), new.weight, main = "Cluster reconstructed mean weigth vs cluster reconstructed mean weigth")
    abline(0, 1) # yes
    #
  }


  cluster$sigmaindiv.anch.age.0 <- cluster.age.0$sigmaindiv.anch.age.0 <- cluster.age.1$sigmaindiv.anch.age.0 <- apply(sigma.ind.matrix * as.matrix(cluster.age.0[, match("L1", names(cluster.age.0)):match("L20", names(cluster.age.0))]), FUN = sum, MAR = 1)


  cluster$sigmaindiv.anch.age.1 <- cluster.age.0$sigmaindiv.anch.age.1 <- cluster.age.1$sigmaindiv.anch.age.1 <- apply(sigma.ind.matrix * as.matrix(cluster.age.1[, match("L1", names(cluster.age.1)):match("L20", names(cluster.age.1))]), FUN = sum, MAR = 1)


  # So I need to calculate the proportions
  # the proportions are given by sigma_age.01*num_age.0 / (sigma_age1*num_age1 + sigma_age.0*num_age.0)

  cluster$prop.anch.age.0 <- cluster.age.0$prop.anch.age.0 <- cluster.age.1$prop.anch.age.0 <- cluster$num.anch.age.0 * cluster$sigmaindiv.anch.age.0 / (cluster$num.anch.age.0 * cluster$sigmaindiv.anch.age.0 + cluster$num.anch.age.1 * cluster$sigmaindiv.anch.age.1) * cluster$prop.anch
  cluster$prop.anch.age.1 <- cluster.age.0$prop.anch.age.1 <- cluster.age.1$prop.anch.age.1 <- cluster$num.anch.age.1 * cluster$sigmaindiv.anch.age.1 / (cluster$num.anch.age.0 * cluster$sigmaindiv.anch.age.0 + cluster$num.anch.age.1 * cluster$sigmaindiv.anch.age.1) * cluster$prop.anch

  cluster$prop.anch.age.0[is.na(cluster$prop.anch.age.0)] <- 0
  cluster$prop.anch.age.1[is.na(cluster$prop.anch.age.1)] <- 0

  cluster.age.0$prop.anch.age.0[is.na(cluster.age.0$prop.anch.age.0)] <- 0
  cluster.age.0$prop.anch.age.1[is.na(cluster.age.0$prop.anch.age.1)] <- 0

  cluster.age.1$prop.anch.age.0[is.na(cluster.age.1$prop.anch.age.0)] <- 0
  cluster.age.1$prop.anch.age.1[is.na(cluster.age.1$prop.anch.age.1)] <- 0


  cluster$sigmaindiv.anch.age.0[cluster$prop.anch.age.0 == 0] <- 1
  cluster$sigmaindiv.anch.age.1[cluster$prop.anch.age.1 == 0] <- 1

  cluster.age.0$sigmaindiv.anch.age.0[cluster.age.0$prop.anch.age.0 == 0] <- 1
  cluster.age.0$sigmaindiv.anch.age.1[cluster.age.0$prop.anch.age.1 == 0] <- 1


  cluster.age.1$sigmaindiv.anch.age.0[cluster.age.1$prop.anch.age.0 == 0] <- 1
  cluster.age.1$sigmaindiv.anch.age.1[cluster.age.1$prop.anch.age.1 == 0] <- 1




  ## sigmas


  #  I need to check that 1 / sigma == prop_age0 * 1/sigma_age0  +  prop_age1 * 1/sigma_age1


  plot(round(1 / cluster$sigmawg.anch * cluster$prop.anch), round(cluster$prop.anch.age.1 * 1 / cluster$sigmaindiv.anch.age.1 * cluster$meanwg.anch.age.1 + cluster$prop.anch.age.0 * 1 / cluster$sigmaindiv.anch.age.0 * cluster$meanwg.anch.age.0))
  abline(0, 1)
  abline(0, 1.05, lty = 2)
  abline(0, 0.95, lty = 2)

  # need to compute sigmawg.anch.age0 and sigmawg.anch.age1

  ## aqui
  cluster$sigmwg.anch.age.0 <- cluster.age.0$sigmwg.anch.age.0 <- cluster.age.1$sigmwg.anch.age.0 <- cluster$sigmaindiv.anch.age.0 / cluster$meanwg.anch.age.0
  cluster$sigmwg.anch.age.0[cluster$prop.anch.age.0 == 0] <- 1
  cluster.age.0$sigmwg.anch.age.0[cluster.age.0$prop.anch.age.0 == 0] <- 1
  cluster.age.1$sigmwg.anch.age.0[cluster.age.1$prop.anch.age.0 == 0] <- 1


  cluster$sigmwg.anch.age.1 <- cluster.age.0$sigmwg.anch.age.1 <- cluster.age.1$sigmwg.anch.age.1 <- cluster$sigmaindiv.anch.age.1 / cluster$meanwg.anch.age.1
  cluster$sigmwg.anch.age.1[cluster$prop.anch.age.1 == 0] <- 1
  cluster.age.0$sigmwg.anch.age.1[cluster.age.0$prop.anch.age.1 == 0] <- 1
  cluster.age.1$sigmwg.anch.age.0[cluster.age.1$prop.anch.age.0 == 0] <- 1



  names(cluster)
  par(mfrow = c(1, 2))
  plot(round(1 / cluster$sigmawg.anch * cluster$prop.anch), round(cluster$prop.anch.age.1 * 1 / cluster$sigmwg.anch.age.1 + cluster$prop.anch.age.0 * 1 / cluster$sigmwg.anch.age.0)) # it confirms that the sum of the age 0 and age 1 densitie is similar to that of the total

  abline(0, 1)
  abline(0, 1.05, lty = 2)
  abline(0, 0.95, lty = 2)


  plot(round(1 / new.sigma.wg * cluster$prop.anch), round(cluster$prop.anch.age.1 * 1 / cluster$sigmwg.anch.age.1 + cluster$prop.anch.age.0 * 1 / cluster$sigmwg.anch.age.0)) # it confirms that the sum of the age 0 and age 1 densitie is similar to that of the total

  abline(0, 1)
  abline(0, 1.05, lty = 2)
  abline(0, 0.95, lty = 2)

  return(list(cluster = cluster, cluster.age.0 = cluster.age.0, cluster.age.1 = cluster.age.1))
}
