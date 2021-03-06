actions <- c("N", "S", "E", "W")

x <- 1:5
y <- 1:4

rewards <- matrix(rep(0, 20), nrow=4)
rewards[1, 5] <- 100
rewards[4, 5] <- -100

#rewards[1, 4] <- 1
#rewards[2, 4] <- -1

values <- rewards # initial values
values[values < 1000] = 0

states <- expand.grid(x=x, y=y)

# Transition probability
transition <- list("N" = c("N" = 1, "S" = 0, "E" = 0, "W" = 0), 
                   "S"= c("S" = 1, "N" = 0, "E" = 0, "W" = 0),
                   "E"= c("E" = 1, "W" = 0, "S" = 0, "N" = 0),
                   "W"= c("W" = 1, "E" = 0, "S" = 0, "N" = 0))

# The value of an action (e.g. move north means y + 1)
action.values <- list("N" = c("x" = 0, "y" = 1), 
                      "S" = c("x" = 0, "y" = -1),
                      "E" = c("x" = -1, "y" = 0),
                      "W" = c("x" = 1, "y" = 0))

# act() function serves to move the robot through states based on an action
act <- function(action, state) {
  action.value <- action.values[[action]]
  new.state <- state
  #
  if(state["x"] == 5 && state["y"] == 1 || state["x"] == 5 && state["y"] == 4)
    return(state)
  #
  new.x = state["x"] + action.value["x"]
  new.y = state["y"] + action.value["y"]
  # Constrained by edge of grid
  new.state["x"] <- min(x[length(x)], max(x[1], new.x))
  new.state["y"] <- min(y[length(y)], max(y[1], new.y))
  #
  if(is.na(rewards[new.state["y"], new.state["x"]]))
    new.state <- state
  #
  return(new.state)
}

bellman.update <- function(action, state, values, gamma=1) {
  state.transition.prob <- transition[[action]]
  q <- rep(0, length(state.transition.prob))
  for(i in 1:length(state.transition.prob)) {        
    new.state <- act(names(state.transition.prob)[i], state) 
    # TD learning
    q[i] <- (state.transition.prob[i] * (rewards[new.state["y"], new.state["x"]] + (gamma * values[new.state["y"], new.state["x"]])))
    #print("prob\n")
    #   print(state.transition.prob[i])
    #    print("rewards")
    # print(rewards[new.state["y"], new.state["x"]] )
    
  }
  sum(q)
}

value.iteration <- function(states, actions, rewards, values, gamma, niter) {
  for (j in 1:niter) {
    for (i in 1:nrow(states)) {
      state <- unlist(states[i,])
      if(i %in% c(5, 20)) next # terminal states
      q.values <- as.numeric(lapply(actions, bellman.update, state=state, values=values, gamma=gamma))
      values[state["y"], state["x"]] <- max(q.values)
      print(values)
    }
  }
  return(values)
}

final.values <- value.iteration(states=states, actions=actions, rewards=rewards, values=values, gamma=0.9, niter=10)

