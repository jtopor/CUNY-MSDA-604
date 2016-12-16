# sim parallel
source("single_sim.R")  # import single_sim... 

library(parallel)
library(foreach)
library(doParallel)

#setup code
free_inches <- seq(3, 6, 0.5)
df_dim <- length(free_inches)
res_master <- data.frame(Inches = free_inches, mu = numeric(df_dim), mdn = numeric(df_dim),
                         min = numeric(df_dim), max = numeric(df_dim))
sim_results = numeric(100)
del_bks = numeric(100)

df_ind <- 1

bxp_df <- data.frame(matrix(, nrow=100, ncol=0))
delbks_df <- data.frame(matrix(, nrow=100, ncol=0))

# setup parallel code
no_cores = detectCores() - 1  # Calculate the number of cores
cl = makeCluster(no_cores)  # Initiate cluster
registerDoParallel(cl)

#start = Sys.time()
for (k in free_inches) {
    # run 100 iterations for each free shelf space value
    result = foreach(icount(100))  %dopar% {
        single_sim(sfree_space = k)
    }
    
    for (i in 1:100){
        sim_results[i] = result[[i]]$iters
        del_bks[i] = result[[i]]$delbks
    }
    
    
    # add col containing sim results to boxplot dataframe
    bxp_df <- cbind(bxp_df, sim_results)
    # set the name of the new col to the number of inches of free space
    colnames(bxp_df)[colnames(bxp_df) == 'sim_results'] <- toString(k)
    
    # add col containing del_bks to deleted books dataframe
    delbks_df <- cbind(delbks_df, del_bks)
    # set the name of the new col to the number of inches of free space
    colnames(delbks_df)[colnames(delbks_df) == 'del_bks'] <- toString(k)
    
    # display results for free shelf space = k
    print(sprintf("Free Space = %1.1f inches", k))
    print(summary(sim_results))
    xl <- sprintf("Free Space = %1.1f inches", k)
    hist(sim_results, breaks = 20, col = 'yellow', xlab = xl)
    
    # create a boxplot of the results for free shelf space = k
    # boxplot(sim_results, main = xl, col = 'yellow' )
    
    # store results in master results data frame
    res_master$mu[df_ind] <- mean(sim_results)
    res_master$mdn[df_ind] <- median(sim_results)
    res_master$min[df_ind] <- min(sim_results)
    res_master$max[df_ind] <- max(sim_results)
    res_master$mu_delbks[df_ind] <- mean(del_bks)
    
    df_ind <- df_ind + 1
}
#Sys.time() - start

stopImplicitCluster()  # shut down cluster
