library(mHMMbayes)
source('./Functions/insert_nightgap.R')
emotion_data <- readRDS("./data/data_Rowland2020.rds")
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