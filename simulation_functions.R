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
  for (j in (n-r):n){
    pref_temp = contract[contract[,2]==j, ]
    rand = sample(nrow(pref_temp))
    pref_space[[j]] = pref_temp[rand,]
    # add an additional row to prevent the matrix to become a vector
    pref_space[[j]] = rbind(pref_space[[j]], c(999,999,999))
  }
  return(pref_space)
}


##### Deferred Acceptance #####
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
  for (i in 1:n){
    accept_contract = space_acum[[i]][space_acum[[i]][,4]==1,]
    space_choi[i,] = accept_contract
  }
  return(space_choi)
}


##### Benchmark #####
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
  for (i in (n-r):n){
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
  for (i in 1:n){
    accept_contract = space_acum[[i]][space_acum[[i]][,4]==1,]
    space_choi[i,] = accept_contract
  }
  return(space_choi)
}


##### Cumulative #####
algo_cu = function(pref_player, pref_space, n, r, v, 
                   n_seq, r_seq, v_seq, t){
  
  
}

