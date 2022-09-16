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
sim = 1000

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


##### Compare Among Mechanisms #####
# players' preference lists
pref_player = create_player_pref(n, contract)

# space's priority lists
pref_space = create_space_pref(r, n, contract)

# get simulation result
#allocation = algo_da(pref_player, pref_space, n)
allocation = algo_be(pref_player, pref_space, n, r)
#print(allocation)

# calculate the efficiency of the current simulation
# get the ranking for each position
allo = unname(cbind(allocation, rep(0,n)))
# loop over players
for (i in 1:n){
  accept_cont = allo[i,1:3]
  player_id = allo[i,1]
  # loop over spaces
  for (k in 1:nrow(pref_player[[player_id]])){
    # locate the player preference for the allocated space
    if (identical(accept_cont, pref_player[[player_id]][k,])){
      allo[i,5] = k
    }
    else{next}
  }
}
colnames(allo) = c('player', 'space', 'term', 'status', 'ranking')
print(allo)
print(paste('efficiency =', mean(allo[,5])))
rm(allo, allocation, accept_cont, player_id)


##### Testing #####
