# Load libraries
library("devtools")
library("quid")
library("RSQLite")
library("Matrix")

# Open connection with .db file
conn <- dbConnect(RSQLite::SQLite(), "inhibitiontasks.db")

# list with datasets names as strings
list <- paste(rep("dataset", 34), 1:34, sep = "") 

## Calculate Bayes Factors
# For loop that analyzes all datasets and stores results in resultslist
for(i in 1:length(list)){
  sqlStatement <- paste("SELECT * FROM", list[i])
  data <- dbGetQuery(conn, sqlStatement) %>% 
    filter(incl == 1) %>% # filter out rows that shouldn't be included
    mutate(subject = as.factor(subject),
           cond = as.factor(cond))
  assign(paste("res", i, sep = ""), quid:::constraintBF(formula = rt ~ subject*cond,
                                                        data = data,
                                                        whichRandom = "subject",
                                                        ID = "subject",
                                                        iterationsPosterior = 10000,
                                                        whichConstraint = c(cond = "2 > 1"),
                                                        rscaleEffects = c("subject" = 1, "cond" = 1/6, "subject:cond" = 1/10)))
}

# Check results
res1

# Re-calculate Bayes Factors for datasets with a BF of 0, using 20000 iterations instead of 10000
idx <- c(5, 11, 28)
for(i in idx){
  sqlStatement <- paste("SELECT * FROM", list[i])
  data <- dbGetQuery(conn, sqlStatement) %>% 
    filter(incl == 1) %>% # filter out rows that shouldn't be included
    mutate(subject = as.factor(subject),
           cond = as.factor(cond))
  assign(paste("res", i, sep = ""), quid:::constraintBF(formula = rt ~ subject*cond,
                                                        data = data,
                                                        whichRandom = "subject",
                                                        ID = "subject",
                                                        iterationsPosterior = 20000,
                                                        whichConstraint = c(cond = "2 > 1"),
                                                        rscaleEffects = c("subject" = 1, "cond" = 1/6, "subject:cond" = 1/10)))
  }

res5
res11
res28

## Plot results
quid:::plotEffects(res1)

## Estimate the attenuation factor
# list all results
res <- list(res1, res2, res3, res4, res5, res6, res7, res8, res9, res10,
            res11, res12, res13, res14, res15, res16, res17, res18, res19, res20,
            res21, res22, res23, res24, res25, res26, res27, res28, res29, res30,
            res31, res32, res33, res34) 

eta <- vector() # empty vector to store estimated attenuation factors in

for(i in 1:length(res)){
  sigma <- sqrt(mean(res[[i]]@mcmcFull[, "sig2"]))
  sigmatheta <- sqrt(mean(res[[i]]@mcmcFull[, "g_subject_cond"] * res[[i]]@mcmcFull[, "sig2"]))
  eta[i] <- sigmatheta/sigma
}

## Save results
save(res, file = "results.rda")
