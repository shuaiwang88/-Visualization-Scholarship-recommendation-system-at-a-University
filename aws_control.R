#### This is for mananging the shiny upload using the package ramazon
#install.packages("devtools")
library(devtools)
#install_github("andreacirilloac/ramazon")
library(ramazon)

ramazon(public_DNS ="ec2-54-208-72-116.compute-1.amazonaws.com",key_pair_name = "Shiny",test = FALSE)
