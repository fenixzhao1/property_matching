##### Preparation #####
# add package
rm(list = ls())
library(ggplot2)
library(xtable)
library(dplyr)
library(here)

# import functions
source(here("simulation_functions.R"))

# players' ID
n = 5 # number of players and total spaces
r = 2 # number of residents and their spaces
v = n-r # number of visitors and their spaces
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
num_contract = nrow(contract)


##### Testing Algorithms #####
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

# get the initial empty accumulate set for each space
space_acum = pref_space
player_acum = pref_player

for (i in 1:n){
  # add a 4th column to store whether the contract is in the accumulate set
  space_acum[[i]] = cbind(space_acum[[i]], rep(0, nrow(space_acum[[i]])))
}

# get the initial empty choice set for each space
space_choi = matrix(0, nrow = n, ncol = 4)

# loop for Benchmark mechanism
for (i in 1:1000){
  
  # collect the players without contract
  reject_player = c()
  for (j in 1:n){
    if (j %in% space_choi[,1]){next}
    else{reject_player = c(reject_player, j)}
  }
  
  # break out of the loop if there is no rejected player
  if (length(reject_player)==0){
    break}
  # randomly select a rejected player to propose the choice
  else if (length(reject_player)==1){
    p = reject_player
  }
  else{
    p = sample(reject_player, 1)
  }

  # skip if the player only has one last invalid contract
  if (is.null(nrow(player_acum[[p]]))){next}
  # get that player's most preferred contract and remove it from the preference
  contract = player_acum[[p]][1,]
  player_acum[[p]] = player_acum[[p]][-1,]
  
  # checkpoint
  print(paste('loop', i, 'start'))
  print(contract)
  
  # locate the corresponding space and add the contract to space' accumulate set
  # =1 means the contract is in the accumulate set
  # =2 means the contract is blocked by a claim contract
  s = contract[2]
  for (k in 1:nrow(space_acum[[s]])){
    if (identical(contract, space_acum[[s]][k,1:3]) & space_acum[[s]][k,4]!=2){
      space_acum[[s]][k,4] = 1
      break
    }
    else{next}
  }
  
  # for the space selected update the corresponding choice set
  accept_contract = space_acum[[s]][space_acum[[s]][,4]==1,]
  if (is.null(nrow(accept_contract))){
    space_choi[s,] = accept_contract
  }
  else if(nrow(accept_contract)>1){
    accept_contract = accept_contract[1,]
    space_choi[s,] = accept_contract
  }
  else{next}
  
  # checkpoint
  print(paste('i =', i, 'CP1'))
  print(space_choi)
  
  # if the player is resident lock the resident's space with t- contract
  if (p<=r & accept_contract[1]==p & accept_contract[2]!=p & accept_contract[3]==0){
    # send back a claim contract to block the t+ term of the resident's space
    for (k in 1:nrow(space_acum[[p]])){
      if (space_acum[[p]][k,1]!=p & space_acum[[p]][k,3]==1)
        # mark the claim contract status = 2
      {space_acum[[p]][k,4]=2}
      else{next}
    }
    # the resident's space rerun its choice function
    rerun_contract = space_acum[[p]][space_acum[[p]][,4]==1,]
    if (is.null(nrow(rerun_contract))){
      space_choi[p,] = rerun_contract
    }
    else if (nrow(rerun_contract)>1){
      rerun_contract = rerun_contract[1,]
      space_choi[p,] = rerun_contract
    }
    else{
      space_choi[p,] = c(0,0,0,0)
    }
    
    # checkpoint
    print(paste('i =', i, 'CP2'))
    print(space_choi)
  }
  
  # anyone who hold two contracts, she keeps the preferred one.
  # loop over players in space choices to find the player with two choices
  for (k in 1:n){
    player_hold = space_choi[space_choi[,1]==k,]
    nrow = ifelse(is.null(nrow(player_hold)), 1, nrow(player_hold))
    # if one player has two accepted contracts (at most two)
    if (nrow>=2){
      # extract the two contracts
      contract1 = player_hold[1,]
      contract2 = player_hold[2,]
      # loop over players preference to compare the two contracts
      for (m in 1:nrow(pref_player[[k]])){
        # if player prefers contract1, remove contract2 from space choice
        if (identical(contract1[1:3], pref_player[[k]][m,])){
          sp = contract2[2]
          for (l in 1:nrow(space_choi)){
            if (identical(contract2[1:3], space_choi[l,1:3]))
            {space_choi[l,1:4]=c(0,0,0,0)
            break}
            else{next}
          }
          # remove contract2 from space accumulate set
          for (l in 1:nrow(space_acum[[sp]])){
            if (identical(contract2[1:3], space_acum[[sp]][l,1:3]))
            {space_acum[[sp]][l,4]=0
            break}
            else{next}
          }
          # break out of the loop
          break
        }
        # if player prefers contract2, remove contract1 from space choice
        else if (identical(contract2[1:3], pref_player[[k]][m,])){
          sp = contract1[2]
          for (l in 1:nrow(space_choi)){
            if (identical(contract1[1:3], space_choi[l,1:3]))
            {space_choi[l,1:4]=c(0,0,0,0)
            break}
            else{next}
          }
          # remove contract1 from space accumulate set
          for (l in 1:nrow(space_acum[[sp]])){
            if (identical(contract1[1:3], space_acum[[sp]][l,1:3]))
            {space_acum[[sp]][l,4]=0
            break}
            else{next}
          }
          # break out of the loop
          break
        }
        else{next}
      }
    }
  }
  
  # checkpoint
  print(paste('i =', i, 'CP3'))
  print(space_choi)
}

# return the allocation
colnames(space_choi) = c('player', 'space', 'term', 'status')
print(paste('i =', i))
print(space_choi)


##### Compare Among Mechanisms #####
# create data container
table = matrix(0, nrow = 3, ncol = 5)
rownames(table) = c('#steps', 'Efficiency', 'Property Right Violation')
colnames(table) = c('Deferred Acceptance', 'Benchmark',
                    'Cumulative Offer', 'Separate Matching',
                    'DA with protection')

# Run simulations
for (i in 1:sim){
  
  # players' preference lists
  pref_player = create_player_pref(n, contract)
  
  # space's priority lists
  pref_space = create_space_pref(r, n, contract)
  
  # get simulation result
  allo_da = algo_da(pref_player, pref_space, n)
  allo_be = algo_be(pref_player, pref_space, n, r)
  allo_co = algo_co(pref_player, pref_space, n, r)
  allo_sm = algo_sm(pref_player, pref_space, n, r)
  allo_dapro = algo_da_pro(pref_player, pref_space, n, r)
  
  # update the number of steps
  table[1,1] = table[1,1] + allo_da[[1]]
  table[1,2] = table[1,2] + allo_be[[1]]
  table[1,3] = table[1,3] + allo_co[[1]]
  table[1,4] = table[1,4] + allo_sm[[1]]
  table[1,5] = table[1,5] + allo_dapro[[1]]
  
  # efficiency is the sum of the rankings of all the players
  # DA efficiency
  effi_da = 0
  allo = unname(allo_da[[2]])
  for (i in 1:n){
    accept_cont = allo[i,1:3]
    player_id = allo[i,1]
    if (player_id == 0){
      effi_da = effi_da + nrow(pref_player[[1]])
      break
    }
    for (k in 1:nrow(pref_player[[player_id]])){
      if (identical(accept_cont, pref_player[[player_id]][k,])){
        effi_da = effi_da + k
        break
      }
      else{next}
    }
  }
  
  # BE efficiency
  effi_be = 0
  allo = unname(allo_be[[2]])
  for (i in 1:n){
    accept_cont = allo[i,1:3]
    player_id = allo[i,1]
    if (player_id == 0){
      effi_be = effi_be + nrow(pref_player[[1]])
      break
    }
    for (k in 1:nrow(pref_player[[player_id]])){
      if (identical(accept_cont, pref_player[[player_id]][k,])){
        effi_be = effi_be + k
        break
      }
      else{next}
    }
  }
  
  # CO efficiency
  effi_co = 0
  allo = unname(allo_co[[2]])
  for (i in 1:n){
    accept_cont = allo[i,1:3]
    player_id = allo[i,1]
    if (player_id == 0){
      effi_co = effi_co + nrow(pref_player[[1]])
      break
    }
    for (k in 1:nrow(pref_player[[player_id]])){
      if (identical(accept_cont, pref_player[[player_id]][k,])){
        effi_co = effi_co + k
        break
      }
      else{next}
    }
  }
  
  # SM efficiency
  effi_sm = 0
  allo = unname(allo_sm[[2]])
  for (i in 1:n){
    accept_cont = allo[i,1:3]
    player_id = allo[i,1]
    if (player_id == 0){
      effi_sm = effi_sm + nrow(pref_player[[1]])
      break
    }
    for (k in 1:nrow(pref_player[[player_id]])){
      if (identical(accept_cont, pref_player[[player_id]][k,])){
        effi_sm = effi_sm + k
        break
      }
      else{next}
    }
  }
  
  # DA pro efficiency
  effi_dapro = 0
  allo = unname(allo_dapro[[2]])
  for (i in 1:n){
    accept_cont = allo[i,1:3]
    player_id = allo[i,1]
    if (player_id == 0){
      effi_dapro = effi_dapro + nrow(pref_player[[1]])
      break
    }
    for (k in 1:nrow(pref_player[[player_id]])){
      if (identical(accept_cont, pref_player[[player_id]][k,])){
        effi_dapro = effi_dapro + k
        break
      }
      else{next}
    }
  }
  
  # update the efficiency in the table
  table[2,1] = table[2,1] + effi_da/effi_da
  table[2,2] = table[2,2] + effi_da/effi_be
  table[2,3] = table[2,3] + effi_da/effi_co
  table[2,4] = table[2,4] + effi_da/effi_sm
  table[2,5] = table[2,5] + effi_da/effi_dapro
  
  # update property right violation rate
  # DA violation
  violation = rep(0, r)
  allo = unname(allo_da[[2]])
  for (i in 1:r){
    resi_contract = allo[allo[,1]==i]
    if (length(resi_contract)==0){
      violation[i] = 1
    }
    else{
      if (resi_contract[2]!=i & resi_contract[3]==0 & allo[i,3]==1){
        violation[i] = 1
      }
    }
  }
  vio_da = ifelse(sum(violation)>0, 1, 0)
  
  # BE violation
  violation = rep(0, r)
  allo = unname(allo_be[[2]])
  for (i in 1:r){
    resi_contract = allo[allo[,1]==i]
    if (length(resi_contract)==0){
      violation[i] = 1
    }
    else{
      if (resi_contract[2]!=i & resi_contract[3]==0 & allo[i,3]==1){
        violation[i] = 1
      }
    }
  }
  vio_be = ifelse(sum(violation)>0, 1, 0)
  
  # CO violation
  violation = rep(0, r)
  allo = unname(allo_co[[2]])
  for (i in 1:r){
    resi_contract = allo[allo[,1]==i]
    if (length(resi_contract)==0){
      violation[i] = 1
    }
    else{
      if (resi_contract[2]!=i & resi_contract[3]==0 & allo[i,3]==1){
        violation[i] = 1
      }
    }
  }
  vio_co = ifelse(sum(violation)>0, 1, 0)
  
  # SM violation
  violation = rep(0, r)
  allo = unname(allo_sm[[2]])
  for (i in 1:r){
    resi_contract = allo[allo[,1]==i]
    if (length(resi_contract)==0){
      violation[i] = 1
    }
    else{
      if (resi_contract[2]!=i & resi_contract[3]==0 & allo[i,3]==1){
        violation[i] = 1
      }
    }
  }
  vio_sm = ifelse(sum(violation)>0, 1, 0)
  
  # DA pro violation
  violation = rep(0, r)
  allo = unname(allo_dapro[[2]])
  for (i in 1:r){
    resi_contract = allo[allo[,1]==i]
    if (length(resi_contract)==0){
      violation[i] = 1
    }
    else{
      if (resi_contract[2]!=i & resi_contract[3]==0 & allo[i,3]==1){
        violation[i] = 1
      }
    }
  }
  vio_dapro = ifelse(sum(violation)>0, 1, 0)
  
  # update the violation rate in the table
  table[3,1] = table[3,1] + vio_da
  table[3,2] = table[3,2] + vio_be
  table[3,3] = table[3,3] + vio_co
  table[3,4] = table[3,4] + vio_sm
  table[3,5] = table[3,5] + vio_dapro
}

# calculate the average
table = round(table/sim, digits = 3)
print(table)


##### Results for One Single Trail #####
# players' preference lists
pref_player = create_player_pref(n, contract)

# space's priority lists
pref_space = create_space_pref(r, n, contract)

# get simulation result
allo_da = algo_da(pref_player, pref_space, n)
allo_be = algo_be(pref_player, pref_space, n, r)
allo_co = algo_co(pref_player, pref_space, n, r)
allo_sm = algo_sm(pref_player, pref_space, n, r)

# print the result
print(pref_player)
print(pref_space)
print(allo_da[[2]])
print(allo_be[[2]])
print(allo_co[[2]])
print(allo_sm[[2]])