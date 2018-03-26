#Define all the functions that will be used

genKids = 
  function(bTime, cTime, lambda = 0.5, kappa = 0.3) 
  {
    # Parent job born at bTime and completes at cTime
    
    # Birth time of first child
    mostRecent = rexp(1, rate = lambda) + bTime
    kidBirths = numeric()
    
    while (mostRecent < cTime) {
      kidBirths = c(kidBirths, mostRecent)
      mostRecent = mostRecent  + rexp(1, rate = lambda) 
    }
    
    # generate lifetimes for all offspring
    numKids = length(kidBirths)
    runtime = rexp(numKids, rate = kappa)
    kidCompletes = rep(cTime, numKids) + runtime
    
    data.frame(births = kidBirths, 
               completes = kidCompletes)
  }

genBirth = function(currentTime, cTime, 
                    births = numeric(), lambda = 0.5) {
  
  # Generate birth time of next job after currentTime
  mostRecent = rexp(1, rate = lambda) + currentTime
  
  if (mostRecent > cTime) 
    return(births)
  else {
    births = c(births, mostRecent)
    genBirth(currentTime = mostRecent, cTime, births, lambda)
  }
}

genKidsR = 
  function(bTime, cTime, lambda = 0.5, kappa = 0.3) {
    # Parent job born at bTime and completes at cTime
    
    kidBirths = genBirth(bTime, cTime, lambda = lambda)
    
    # generate lifetimes for all offspring
    numKids = length(kidBirths)
    runtime = rexp(numKids, rate = kappa)
    kidDeaths = rep(cTime, numKids) + runtime
    
    data.frame(births = kidBirths, 
               completes = kidDeaths)
  } 

genKidsU = 
  function(bTime, cTime, lambda = 0.5, kappa = 0.3) {
    # Generate the birth times and assassination times
    # for the children of a job who is born at bTime 
    # and completed at cTime.
    lambda = (cTime - bTime) * lambda
    numKids = rpois(1, lambda = lambda)
    kidBirths = sort(runif(numKids, min = bTime, max = cTime))
    
    # generate lifetimes for each offspring
    runtime = rexp(numKids, rate = kappa)
    kidDeaths = rep(cTime, numKids) + runtime
    
    return(data.frame(births = kidBirths, completes = kidDeaths))
  }

genKidsV = function(bTimes, cTimes, parentID, lambda = 0.5, kappa = 0.3) {
  # Determine how many children each job has
  parentAge = cTimes - bTimes
  numKids = rpois(n = length(parentAge), lambda = lambda * parentAge)
  
  if (sum(numKids) == 0) return(NULL)
  
  # Determine the birth times of the children  
  kidStats = 
    mapply(function(n, min, max) {
      births = sort(runif(n, min, max))
      runtimes = rexp(n, rate = kappa)
      completes = rep(max, n) + runtimes
      data.frame(births, completes)
    },
    n = numKids , min = bTimes, max = cTimes, 
    SIMPLIFY = FALSE)
  
  
  return(data.frame(parentID = rep(parentID, numKids),
                    kidID = 1:sum(numKids), 
                    births = unlist(lapply(kidStats, "[[", "births")), 
                    completes = unlist(lapply(kidStats,"[[", "completes"))
  ))
}

familyTree = function(lambda = 0.5, kappa = 0.3, 
                      maxGen = 10, maxOffspring = 1000) {
  
  # Return value - a list with 1 data frame per generation.
  allGens = vector(mode = "list", length = maxGen)
  
  # Generate root of the tree
  allGens[[1]] = data.frame(parentID = NA, kidID = 1, 
                            births = 0, 
                            completes = rexp(1, rate = kappa))
  
  currentNumOffspring = 0
  
  # Generate future generations, one at a time.
  for (i in 2:maxGen) {
    nextGen = genKidsV(bTimes = allGens[[ (i - 1) ]]$births,
                       cTimes = allGens[[ (i - 1) ]]$completes,
                       parentID = allGens[[ (i - 1) ]]$kidID,
                       lambda = lambda, kappa = kappa)
    if (is.null(nextGen)) return(allGens[ 1:(i - 1) ]) 
    allGens[[ i ]] = nextGen
    currentNumOffspring = currentNumOffspring + nrow(nextGen)
    if (currentNumOffspring > maxOffspring) 
      return(allGens[1:i])
  }  
  allGens
}