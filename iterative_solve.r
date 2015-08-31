#solver helper functions 
solverSetupData <- function(df){
  
  #soft lock players
  df$fp <- ifelse(df$lock == T, df$fp * 4, df$fp)
  
  #remove excluded players
  df <- df[df$exclude == F, ]
  
  #setup player dummy variables
  dummy_vars <- matrix(as.numeric(model.matrix(fp ~ ps, data = df)), ncol = 5)
  
  #setup positional dummy vars
  dummy_vars[, 1] <- ifelse(df$ps == "C", 1, 0)
  dummy_vars <- cbind(dummy_vars, ifelse(df$ps == "PG" | df$ps == "SG", 1, 0))
  dummy_vars <- cbind(dummy_vars, ifelse(df$ps == "SF" | df$ps == "PF", 1, 0))
  
  # copy matrix to set min values
  dummy_vars <- cbind(dummy_vars, dummy_vars)
  
  # add in salary and ppg info 
  dummy_vars <- cbind(dummy_vars, 1, df$salary, df$fp, df$fp)
  cnames <- c("c", "pf", "pg", "sf", "sg", "g", "f", 
              "c", "pf", "pg", "sf", "sg", "g", "f",
              "tot", "salary", "fp_const", "fp")
  
  colnames(dummy_vars) <- cnames
  dummy_vars <- t(dummy_vars)
  
  dummy_vars
}

runSolver <- function(df, dummy_vars, max_fp = 1000000000){
  #remove excluded players
  df <- df[df$exclude == F, ]
  df$fp <- ifelse(df$lock == T, df$fp * 4, df$fp)
  
  # max_fp <- 100
  # character vector of direction constraints
  dir <- c("<=", "<=", "<=", "<=", "<=", "<=", "<=", 
           ">=", ">=", ">=", ">=", ">=", ">=", ">=",
           "==", "<=", "<")
  # right hand side constraints
  # max number of players at each position [1-5]
  # 3 guards and 3 fowrads [6-7]
  # total number selected 
  rhs <- c(2, 3, 3, 3, 3, 4, 4, 
           # min players at each position
           1, 1, 1, 1, 1, 3, 3,
           # max players and max salary
           8, 50000, 
           # max fantasy points
           max_fp)
  
  # maximize or minimize the problem
  max <- "max"
  
  # binary objective variables
  types <- T
  
  # Objective values to maximize
  obj <- dummy_vars[18, ]
  
  # matrix
  mat <- dummy_vars[1:17, ]
  
  # setup the solver
  sol <- lp(objective.in = obj,
            const.mat = mat,
            const.dir = dir,
            const.rhs = rhs,
            all.bin = types,
            direction = max)
  
  # examine selected team
  df$selected <- sol$solution
  team_out <- df[df$selected == 1, ]
  
  #filter return columns
  team_out <- team_out %>%
    select(name, ps, salary, game_info, fp, lock)
  team_out$opt_sol <- sum(team_out$fp)
  team_out$fp <- ifelse(team_out$lock == T, team_out$fp / 4, team_out$fp)
  team_out$proj_fp <- sum(team_out$fp)

  #write out selected team
  team_out
}

loopSolver <- function(df, dummy_vars, loops){
  # loops <- 20
  out <- data.frame()
  
  for(i in 1:loops){
    tmp <- runSolver(df, dummy_vars, max = ifelse(i == 1, 1000, opt_sol - .1))
    opt_sol <- unique(tmp$opt_sol)
    tmp$opt_sol <- NULL
    tmp$lock <- NULL
    tmp$sol_num <- i
    out <- rbind(out, tmp)
#     flush.console()
#     print(i)
  }
  out
}