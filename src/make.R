# make file for Case Study


# only dropped in what we need for any of the questions, even some
# of this is a bit redundant

# vector generate children in BA process
genKidsV = function(bTimes, cTimes, parentID, lambda = 0.5, kappa = 0.3) {
  # Determine how many children each job has
  parentAge = cTimes - bTimes
  # get interarrival times (births)
  numKids = rpois(n = length(parentAge), lambda = lambda * parentAge)
  # if for some reason no children, return NULL
  if (sum(numKids) == 0) return(NULL)
  
  # Determine the birth times of the children  
  # use mapply because runif not vectorizable
  # pass in vectors of numKids, birthTimes and completeTimes
  kidStats = 
    mapply(function(n, min, max) {
      # get birth times for number of kids as part of uniform dist.
      # sort them ascending
      births = sort(runif(n, min, max))
      # get lifetimes as exponential
      runtimes = rexp(n, rate = kappa)
      # add it to parent's complete time (max)
      completes = rep(max, n) + runtimes
      # wrap it in a df
      data.frame(births, completes)
    },
    n = numKids , min = bTimes, max = cTimes, 
    SIMPLIFY = FALSE)
  
  # return IDs for parents, kids, births and completes
  return(data.frame(parentID = rep(parentID, numKids),
                    kidID = 1:sum(numKids), 
                    births = unlist(lapply(kidStats, "[[", "births")), 
                    completes = unlist(lapply(kidStats,"[[", "completes"))
  ))
}

# use familyTree with maxGen and maxOffspring params
# this function grows a tree instead of applying directly
# to genKidsV
familyTree = function(lambda = 0.5, kappa = 0.3, 
                      maxGen = 10, maxOffspring = 1000) {
  
  # Return value - a list with 1 data frame per generation.
  allGens = vector(mode = "list", length = maxGen)
  
  # Generate root of the tree
  allGens[[1]] = data.frame(parentID = NA, kidID = 1, 
                            births = 0, # parent is born at 0
                            completes = rexp(1, rate = kappa)) # parent complete 
  
  # sentry for number of offspring
  currentNumOffspring = 0
  
  # Generate future generations, one at a time.
  for (i in 2:maxGen) {
    nextGen = genKidsV(bTimes = allGens[[ (i - 1) ]]$births, # pass vector of births
                       cTimes = allGens[[ (i - 1) ]]$completes, # pass vector of completes
                       parentID = allGens[[ (i - 1) ]]$kidID, # pass kidID from prev
                       lambda = lambda, 
                       kappa = kappa) # lambda and kappas for kids and lifetimes
    
    # if nextGen DF is null, return all before current
    if (is.null(nextGen)) return(allGens[ 1:(i - 1) ]) 
    # else append this generation to the list
    allGens[[ i ]] = nextGen
    # increment sentry var
    currentNumOffspring = currentNumOffspring + nrow(nextGen)
    # stopping condition for number of offspring
    if (currentNumOffspring > maxOffspring) 
      return(allGens[1:i])
  }  
  allGens
}


# iterates over each generation to get num rows
# takes length of entire df to agg num gens
exptOne = function(l, k, mG, mO){
  # Helper function to call familyTree
  # Returns - summary statistics for analysis,
  
  aTree = familyTree(lambda = l, kappa = k, maxGen = mG,
                     maxOffspring = mO)
  numGen = length(aTree)
  numJobs = sum(sapply(aTree, nrow))
  c(numGen, numJobs)
}



# MCBA takes a parameter matrix to iterate over
# it also implements a repeat for monte carlo sims per
# parameter combination
# returns a matrix with cols == number of reps, with each
# column containing num gens and num off spring in rows

MCBA = function(params, repeats = 5, mG = 10, mO = 1000){
  # params: matrix columns of lambda and kappa values
  # For each lambda and kappa pair, run "repeats" times
  
  n = nrow(params)
  mcResults = vector("list", length = n)
  
  for (i in 1:n) {
    cat("param set is ", i, "\n")
    mcResults[[i]] = replicate(repeats,
                               exptOne(l = params[i, 1],
                                       k = params[i, 2],
                                       mG = mG, 
                                       mO = mO))
  }
  mcResults
}
