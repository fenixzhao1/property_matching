##### Preparation #####
# add package
rm(list = ls())
library(ggplot2)
library(xtable)
library(dplyr)
library(here)
source(here("simulation_functions.R"))

# players' ID
n = 5 # number of players and total spaces
r = 2 # number of residents and their spaces
v = n-r # number of visitors and their spaces
n_seq = 1:n
r_seq = 1:r
v_seq = (n-r):n
t = c(0,1) # 0 is t- contract and 1 is t+ contract

# contract list
contract = c() # three columns: player id, space id, term
# loop over players
for (i in 1:n){
  # loop over spaces
  for (j in 1:n){
    # loop over terms
    for (k in 1:length(t)){
      contract = rbind(contract, c(i,j,t[k]))
    }
  }
}

# players' preference lists
pref_player = create_player_pref(n, contract)

# space's priority lists
pref_space = create_space_pref(r, n, contract)


##### Data analysis #####
allocation = algo_da(pref_player, pref_space, n)
print(allocation)