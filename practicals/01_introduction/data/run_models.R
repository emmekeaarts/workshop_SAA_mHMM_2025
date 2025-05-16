library(mHMMbayes)
getwd()
source('practicals/Functions/insert_nightgap.R')
emotion_data <- readRDS("practicals/01_introduction/data/data_Rowland2020.rds")
emotion_data <- emotion_data[,c('subj_id', 'dayno', 'beep','happy','relaxed','anxious','depressed')]
emotion_nightgap <- insert_nightgap(data = emotion_data, # make sure the first column contains the subject IDs, and the second column contains the day numbers
                                    beeps_per_night = 8, # we want to insert 8 beeps for each night
                                    ndays = 40 # number of days in the study (constant over subjects)
)
emotion_mHMM <- emotion_nightgap[,c('subj_id', 'happy', 'relaxed', 'anxious', 'depressed')]

m <- 2
n_dep <- 4

start_gamma <- matrix(c(0.7, 0.3, 
                        0.3, 0.7), byrow = TRUE, ncol = m, nrow = m)

start_emiss <- list(matrix(c(70, 15,                                     # happy
                             30, 15), byrow = TRUE, ncol = 2, nrow = m),
                    matrix(c(70, 15,                                     # relaxed
                             30, 15), byrow = TRUE, ncol = 2, nrow = m),
                    matrix(c(15, 10,                                     # anxious
                             50, 15), byrow = TRUE, ncol = 2, nrow = m), 
                    matrix(c(15, 10,                                     # depressed
                             50, 15), byrow = TRUE, ncol = 2, nrow = m))
emotion_prior_emiss <- prior_emiss_cont(
  gen = list(m = m, n_dep = n_dep),
  emiss_mu0 = list(matrix(c(70, 30), nrow = 1),  # happy
                   matrix(c(70, 30), nrow = 1),  # relaxed
                   matrix(c(15, 50), nrow = 1),  # anxious
                   matrix(c(15, 50), nrow = 1)),  # depressed
  emiss_K0 = rep(list(1), n_dep),
  emiss_V = rep(list(rep(5^2, m)), n_dep),
  emiss_nu = rep(list(1), n_dep),
  emiss_a0 = rep(list(rep(1.5, m)), n_dep),
  emiss_b0 = rep(list(rep(20, m)), n_dep),
)
set.seed(42)
out_2st_emotion <- mHMM(s_data = emotion_mHMM,
                        data_distr = "continuous",
                        gen = list(m = m, n_dep = n_dep),
                        start_val = c(list(start_gamma), start_emiss),
                        emiss_hyp_prior = emotion_prior_emiss,
                        mcmc = list(J = 500, burn_in = 200))
saveRDS(out_2st_emotion, file = "practicals/01_introduction/data/out_2st_emotion.rds")

####################
# 3 state model 
####################

## general model properties
m <- 3
n_dep <- 4

## starting values for gamma
start_gamma_3st <- matrix(c(0.8, 0.1, 0.1,
                            0.1, 0.8, 0.1,
                            0.1, 0.1, 0.8), byrow = TRUE, ncol = m, nrow = m)

## starting values for the emission distribution 
start_emiss_3st <- list(matrix(c(80, 10,                                     # happy
                                 50, 10,
                                 20, 10), byrow = TRUE, ncol = 2, nrow = m),
                        matrix(c(80, 10,                                     # relaxed
                                 50, 10,
                                 20, 10), byrow = TRUE, ncol = 2, nrow = m),
                        matrix(c(10,  5,                                     # anxious
                                 30, 10,
                                 50, 15), byrow = TRUE, ncol = 2, nrow = m), 
                        matrix(c(10,  5,                                     # depressed
                                 30, 10,
                                 50, 15), byrow = TRUE, ncol = 2, nrow = m))

## specifying weakly informative prior for continuous emission distributions
emotion_prior_emiss_3st <- prior_emiss_cont(
  gen = list(m = m, n_dep = n_dep),
  emiss_mu0 = list(matrix(c(80, 50, 20), nrow = 1),  # happy
                   matrix(c(80, 50, 20), nrow = 1),  # relaxed
                   matrix(c(10, 30, 50), nrow = 1),  # anxious
                   matrix(c(10, 30, 50), nrow = 1)),  # depressed
  emiss_K0 = rep(list(1), n_dep),
  emiss_V = rep(list(rep(5^2, m)), n_dep),
  emiss_nu = rep(list(1), n_dep),
  emiss_a0 = rep(list(rep(1.5, m)), n_dep),
  emiss_b0 = rep(list(rep(20, m)), n_dep),
)
m <- 3
set.seed(42)
out_3st_emotion <- mHMM(s_data = emotion_mHMM,
                        data_distr = "continuous",
                        gen = list(m = m, n_dep = n_dep),
                        start_val = c(list(start_gamma_3st), start_emiss_3st),
                        emiss_hyp_prior = emotion_prior_emiss_3st,
                        mcmc = list(J = 500, burn_in = 200))
saveRDS(out_3st_emotion, file = "practicals/02_more_advanced/data/out_3st_emotion.rds")

####################
# 4 state model
####################

## general model properties
m <- 4
n_dep <- 4
## starting values for gamma
start_gamma_4st <- matrix(c(0.7, 0.1, 0.1, 0.1,
                            0.1, 0.7, 0.1, 0.1,
                            0.1, 0.1, 0.7, 0.1,
                            0.1, 0.1, 0.1, 0.7), byrow = TRUE, ncol = m, nrow = m)

## starting values for the emission distribution 
start_emiss_4st <- list(matrix(c(80, 10,                                     # happy
                                 60, 10,
                                 40, 10,
                                 20, 10), byrow = TRUE, ncol = 2, nrow = m), 
                        matrix(c(80, 10,                                     # relaxed
                                 60, 10,
                                 40, 10,
                                 20, 10), byrow = TRUE, ncol = 2, nrow = m), 
                        matrix(c(10,  5,                                     # anxious
                                 32, 10,
                                 40, 10,
                                 60, 10), byrow = TRUE, ncol = 2, nrow = m), 
                        matrix(c(10,  5,                                     # depressed
                                 20, 10,
                                 40, 10,
                                 60, 10), byrow = TRUE, ncol = 2, nrow = m))

## specifying weakly informative prior for continuous emission distributions
emotion_prior_emiss_4st <- prior_emiss_cont(
  gen = list(m = m, n_dep = n_dep),
  emiss_mu0 = list(matrix(c(80, 60, 40, 20), nrow = 1),  # happy
                   matrix(c(80, 60, 40, 20), nrow = 1),  # relaxed
                   matrix(c(10, 20, 40, 60), nrow = 1),  # anxious
                   matrix(c(10, 20, 40, 60), nrow = 1)),  # depressed
  emiss_K0 = rep(list(1), n_dep),
  emiss_V = rep(list(rep(5^2, m)), n_dep),
  emiss_nu = rep(list(1), n_dep),
  emiss_a0 = rep(list(rep(1.5, m)), n_dep),
  emiss_b0 = rep(list(rep(20, m)), n_dep)
)

# Fitting the 4 state model 
m <- 4
set.seed(42)
out_4st_emotion <- mHMM(s_data = emotion_mHMM,
                        data_distr = "continuous",
                        gen = list(m = m, n_dep = n_dep),
                        start_val = c(list(start_gamma_4st), start_emiss_4st),
                        emiss_hyp_prior = emotion_prior_emiss_4st,
                        mcmc = list(J = 500, burn_in = 200))
saveRDS(out_4st_emotion, file = "practicals/02_more_advanced/data/out_4st_emotion.rds")


### Extra chains 2-state model
m <- 2
n_dep <- 4

start_gamma_b <- matrix(c(0.9, 0.1, 
                          0.1, 0.9), byrow = TRUE, ncol = m, nrow = m)

start_emiss_b <- list(matrix(c(80, 10,                                     # happy
                               40, 15), byrow = TRUE, ncol = 2, nrow = m), 
                      matrix(c(80, 10,                                     # relaxed
                               40, 15), byrow = TRUE, ncol = 2, nrow = m), 
                      matrix(c(5, 5,                                     # anxious
                               30, 15), byrow = TRUE, ncol = 2, nrow = m), 
                      matrix(c(5, 5,                                     # depressed
                               30, 15), byrow = TRUE, ncol = 2, nrow = m))

set.seed(123)
out_2st_emotion_b <- mHMM(s_data = emotion_mHMM,
                          data_distr = "continuous",
                          gen = list(m = m, n_dep = n_dep),
                          start_val = c(list(start_gamma_b), start_emiss_b),
                          emiss_hyp_prior = emotion_prior_emiss,
                          mcmc = list(J = 500, burn_in = 200))
saveRDS(out_2st_emotion_b, file = "practicals/02_more_advanced/data/out_2st_emotion_b.rds")
