source("input.R")
source("sim_dat.R")
source("i-FWER.R")
source("single_experiment.R")

load_silently = function(packages){
  for(package in packages){
    suppressPackageStartupMessages(library(package, character.only = TRUE))    
  }
  invisible()
}
load_silently(c("magrittr", "splines", "robustbase", "ggplot2"))