library(here)

cluster <- read.csv(here("Output/cluster_length_frequency_Engraulis mordax_2107RL.csv"))
source(here("Sandbox/partition_anchovy_function.R"))

test <- break.down.anchovy(cluster)
# test is a list with three data frames. One for anchovy, another one for anchovy age 0, 
# and the other one for anchovy age 1
names(test)

#original with additional sigmas
test$cluster
# this shows that the proportion of age 0 and age 1 used to break down the acoustic backscatter 
# sums up to the original proportion of anchovy
round(test$cluster$prop.anch - test$cluster$prop.anch.age.0 - test$cluster$prop.anch.age.1 , 5) 

#age 0 lengths with additional sigmas
test$cluster.age.0

#age 1 lengths with additional sigmas
test$cluster.age.1
