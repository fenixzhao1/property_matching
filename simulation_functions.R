##### Create Preference/Priority List #####
create_player_pref = function(n, contract){
  # create empty container
  pref_player = list()
  # loop over players
  for (i in 1:n){
    pref_temp = contract[contract[,1]==i, ]
    rand = sample(nrow(pref_temp))
    pref_player[[i]] = pref_temp[rand,]
    # add an additional row to prevent the matrix to become a vector
    pref_player[[i]] = rbind(pref_player[[i]], c(888,888,888))
  }
  return(pref_player)
}


create_space_pref = function(r, n, contract){
  # create empty container
  pref_space = list()
  # loop over residents' space
  for (j in 1:r){
    pref_temp = contract[contract[,2]==j, ]
    # prioritize resident's contracts
    pref_temp1 = pref_temp[pref_temp[,1]==j, ]
    pref_temp2 = pref_temp[pref_temp[,1]!=j, ]
    rand1 = sample(nrow(pref_temp1))
    rand2 = sample(nrow(pref_temp2))
    pref_space[[j]] = rbind(pref_temp1[rand1,], pref_temp2[rand2,])
    # add an additional row to prevent the matrix to become a vector
    pref_space[[j]] = rbind(pref_space[[j]], c(999,999,999))
  }
  # loop over visitors' space
  for (j in (r+1):n){
    pref_temp = contract[contract[,2]==j, ]
    rand = sample(nrow(pref_temp))
    pref_space[[j]] = pref_temp[rand,]
    # add an additional row to prevent the matrix to become a vector
    pref_space[[j]] = rbind(pref_space[[j]], c(999,999,999))
  }
  return(pref_space)
}


##### Matching Mechanism #####
# Deferred Acceptance
algo_da = function(pref_player, pref_space, n){
  
  # get the initial empty accumulate set for each space
  space_acum = pref_space
  
  for (i in 1:n){
    # add a 4th column to store whether the contract is in the accumulate set
    space_acum[[i]] = cbind(space_acum[[i]], rep(0, nrow(space_acum[[i]])))
  }
  
  # get the initial empty choice set for each space
  space_choi = matrix(0, nrow = n, ncol = 4)
  colnames(space_choi) = c('player', 'space', 'term', 'status')
  
  # loop for GS mechanism
  for (i in 1:10000){
    
    # set up the reject player list for all players in the first round
    if (i==1){
      reject_player = 1:n
    }
    
    # loop over players to submit their preference
    for (j in reject_player){
      # skip if the player only has one last invalid contract
      if (is.null(nrow(pref_player[[j]]))){next}
      # get players' most preferred contract and remove it from the preference
      contract = pref_player[[j]][1,]
      pref_player[[j]] = pref_player[[j]][-1,]
      # locate the corresponding space and add the contract to space' accumulate set
      s = contract[2]
      for (k in 1:nrow(space_acum[[s]])){
        if (identical(contract, space_acum[[s]][k,1:3])){
          space_acum[[s]][k,4] = 1
          break
        }
        else{next}
      }
    }
    
    # initialize reject player for the next round
    reject_player = c()
    
    # loop over spaces to reject the lowest player
    for (j in 1:n){
      # filter the accumulate set
      temp_acum = space_acum[[j]][space_acum[[j]][,4]==1,]
      row = nrow(temp_acum)
      # when row=1, the matrix will be transferred to a vector and nrow() trigger error
      if (is.null(row)){row = 1}
      if (row > 1){
        # find the rejected contract
        reject_contract = temp_acum[row,]
        # add the reject player to the overall reject players set
        reject_player = c(reject_player, temp_acum[row,1])
        # remove the contract from the accumulate set
        for (k in 1:nrow(space_acum[[j]])){
          if (identical(reject_contract, space_acum[[j]][k,])){
            space_acum[[j]][k,4] = 0
            break
          }
          else{next}
        }
      }
      else{next}
    }
    
    # break out of the loop if there is no rejected player
    if (length(reject_player) == 0){break}
    else{next}
  }
  
  # return the allocation
  steps = i
  
  for (i in 1:n){
    accept_contract = space_acum[[i]][space_acum[[i]][,4]==1,]
    space_choi[i,] = accept_contract
  }
  
  my_list = list()
  my_list[[1]] = steps
  my_list[[2]] = space_choi
  return(my_list)
}


# Deferred Acceptance but residents protect their own space
algo_da_pro = function(pref_player, pref_space, n, r){
  
  # get the initial empty accumulate set for each space
  space_acum = pref_space
  player_acum = pref_player
  
  # adjust resident's preference so their prioritize their own space
  for (i in 1:r){
    pref_res = player_acum[[i]][player_acum[[i]][,2]==i,]
    pref_pub = player_acum[[i]][player_acum[[i]][,2]!=i,]
    player_acum[[i]] = rbind(pref_res, pref_pub)
  }
  
  for (i in 1:n){
    # add a 4th column to store whether the contract is in the accumulate set
    space_acum[[i]] = cbind(space_acum[[i]], rep(0, nrow(space_acum[[i]])))
  }
  
  # get the initial empty choice set for each space
  space_choi = matrix(0, nrow = n, ncol = 4)
  colnames(space_choi) = c('player', 'space', 'term', 'status')
  
  # loop for GS mechanism
  for (i in 1:10000){
    
    # set up the reject player list for all players in the first round
    if (i==1){
      reject_player = 1:n
    }
    
    # loop over players to submit their preference
    for (j in reject_player){
      # skip if the player only has one last invalid contract
      if (is.null(nrow(player_acum[[j]]))){next}
      # get players' most preferred contract and remove it from the preference
      contract = player_acum[[j]][1,]
      player_acum[[j]] = player_acum[[j]][-1,]
      # locate the corresponding space and add the contract to space' accumulate set
      s = contract[2]
      for (k in 1:nrow(space_acum[[s]])){
        if (identical(contract, space_acum[[s]][k,1:3])){
          space_acum[[s]][k,4] = 1
          break
        }
        else{next}
      }
    }
    
    # initialize reject player for the next round
    reject_player = c()
    
    # loop over spaces to reject the lowest player
    for (j in 1:n){
      # filter the accumulate set
      temp_acum = space_acum[[j]][space_acum[[j]][,4]==1,]
      row = nrow(temp_acum)
      # when row=1, the matrix will be transferred to a vector and nrow() trigger error
      if (is.null(row)){row = 1}
      if (row > 1){
        # find the rejected contract
        reject_contract = temp_acum[row,]
        # add the reject player to the overall reject players set
        reject_player = c(reject_player, temp_acum[row,1])
        # remove the contract from the accumulate set
        for (k in 1:nrow(space_acum[[j]])){
          if (identical(reject_contract, space_acum[[j]][k,])){
            space_acum[[j]][k,4] = 0
            break
          }
          else{next}
        }
      }
      else{next}
    }
    
    # break out of the loop if there is no rejected player
    if (length(reject_player) == 0){break}
    else{next}
  }
  
  # return the allocation
  steps = i
  
  for (i in 1:n){
    accept_contract = space_acum[[i]][space_acum[[i]][,4]==1,]
    space_choi[i,] = accept_contract
  }
  
  my_list = list()
  my_list[[1]] = steps
  my_list[[2]] = space_choi
  return(my_list)
}


# Benchmark
# similar to DA but the visitors cannot apply t+ contract to residents' space
algo_be = function(pref_player, pref_space, n, r){
  
  # get the initial empty accumulate set for each space
  space_acum = pref_space
  
  for (i in 1:n){
    # add a 4th column to store whether the contract is in the accumulate set
    space_acum[[i]] = cbind(space_acum[[i]], rep(0, nrow(space_acum[[i]])))
  }
  
  # get the initial empty choice set for each space
  space_choi = matrix(0, nrow = n, ncol = 4)
  colnames(space_choi) = c('player', 'space', 'term', 'status')
  
  # remove t+ contract for visitors from resident's space
  for (i in 1:r){
    # mark all the deleted rows (visitors that occupy a resident t+ space)
    for (j in 1:nrow(space_acum[[i]])){
      if (space_acum[[i]][j,1]!=space_acum[[i]][j,2] & space_acum[[i]][j,3]==1)
      {space_acum[[i]][j,4]=2}
    }
    # remove those deleted rows
    space_acum[[i]] = space_acum[[i]][space_acum[[i]][,4]!=2,]
  }
  
  # similarly, remove these contracts from players' preference
  # for residents
  for (i in 1:r){
    pref_player[[i]] = pref_player[[i]][pref_player[[i]][,2]>r | pref_player[[i]][,1]==pref_player[[i]][,2] | pref_player[[i]][,3]==0,]
  }
  # for visitors
  for (i in (r+1):n){
    pref_player[[i]] = pref_player[[i]][pref_player[[i]][,2]>r | pref_player[[i]][,3]==0,]
  }
  
  # loop for Benchmark mechanism
  for (i in 1:10000){
    
    # set up the reject player list for all players in the first round
    if (i==1){
      reject_player = 1:n
    }
    
    # loop over players to submit their preference
    for (j in reject_player){
      # skip if the player only has one last invalid contract
      if (is.null(nrow(pref_player[[j]]))){next}
      # get players' most preferred contract and remove it from the preference
      contract = pref_player[[j]][1,]
      pref_player[[j]] = pref_player[[j]][-1,]
      # locate the corresponding space and add the contract to space' accumulate set
      s = contract[2]
      for (k in 1:nrow(space_acum[[s]])){
        if (identical(contract, space_acum[[s]][k,1:3])){
          space_acum[[s]][k,4] = 1
          break
        }
        else{next}
      }
    }
    
    # initialize reject player for the next round
    reject_player = c()
    
    # loop over spaces to reject the lowest player
    for (j in 1:n){
      # filter the accumulate set
      temp_acum = space_acum[[j]][space_acum[[j]][,4]==1,]
      row = nrow(temp_acum)
      # when row=1, the matrix will be transferred to a vector and nrow() trigger error
      if (is.null(row)){row = 1}
      if (row > 1){
        # find the rejected contract
        reject_contract = temp_acum[row,]
        # add the reject player to the overall reject players set
        reject_player = c(reject_player, temp_acum[row,1])
        # remove the contract from the accumulate set
        for (k in 1:nrow(space_acum[[j]])){
          if (identical(reject_contract, space_acum[[j]][k,])){
            space_acum[[j]][k,4] = 0
            break
          }
          else{next}
        }
      }
      else{next}
    }
    
    # break out of the loop if there is no rejected player
    if (length(reject_player) == 0){break}
    else{next}
  }
  
  # return the allocation
  steps = i
  
  for (i in 1:n){
    accept_contract = space_acum[[i]][space_acum[[i]][,4]==1,]
    space_choi[i,] = accept_contract
  }
  
  my_list = list()
  my_list[[1]] = steps
  my_list[[2]] = space_choi
  return(my_list)
}


# Cumulative Offer
algo_co = function(pref_player, pref_space, n, r){
  
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
  }
  
  # return the allocation
  colnames(space_choi) = c('player', 'space', 'term', 'status')
  
  steps = i
  my_list = list()
  my_list[[1]] = steps
  my_list[[2]] = space_choi
  return(my_list)
}


# Separate Matching
# similar to CO but the players are matched within their own type
algo_sm = function(pref_player, pref_space, n, r){
  
  # get the initial empty accumulate set for each space
  space_acum = pref_space
  player_acum = pref_player
  
  # rebuild the player' preference to separate the markets
  # for residents
  for (i in 1:r){
    player_acum[[i]] = player_acum[[i]][player_acum[[i]][,2]<=r | player_acum[[i]][,2]==888,]
  }
  # for visitors
  for (i in (r+1):n){
    player_acum[[i]] = player_acum[[i]][player_acum[[i]][,2]>r,]
  }
  
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
  }
  
  # return the allocation
  colnames(space_choi) = c('player', 'space', 'term', 'status')
  
  steps = i
  my_list = list()
  my_list[[1]] = steps
  my_list[[2]] = space_choi
  return(my_list)
}
