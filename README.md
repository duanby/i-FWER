# i-FWER
Familywise error rate control via interactive unmasking

## Overview
The i-FWER test is a multiple testing method with FWER control that allows a human analyst to look at (partial) data information and interactively adjust the procedure of excluding possible nulls. 

## Functions
i-FWER.R:    the i-FWER algorithm

sim_dat.R:   simulated data

reproduce.R: reproduce experiments in the paper

## Dependencies
The code was tested on macOS using R (version 3.6.0) and the following packages:
* magrittr
* splines
* robustbase
* ggplot2
