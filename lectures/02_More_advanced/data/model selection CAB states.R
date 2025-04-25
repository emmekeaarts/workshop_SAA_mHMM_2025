library(ggplot2)
library(cowplot)

n_dep <- 5
m <- 2
var_names <- c("self\n control", "negative\n affect", "contact\n avoidance", "contact\n desire", "suicidal\n ideation")
CAB_2st <- data.frame(mean = c(33.10, 48.45, 29.27, 37.12, 38.15, 
                               28.39, 60.15, 40.94, 30.53, 53.27),
                      state = factor(c(rep(1:m, each = n_dep)), levels = 1:m),
                      CAB_factor = factor(rep(var_names, m), levels = var_names))

m <- 3
CAB_3st <- data.frame(mean = c(35.18, 46.93, 24.65, 40.68, 35.83,
                               32.85, 57.70, 38.38, 41.27, 50.27,
                               18.61, 67.46, 46.12, 9.99, 60.18),
                      state = factor(c(rep(1:m, each = n_dep)), levels = 1:m),
                      CAB_factor = factor(rep(var_names, m), levels = var_names))

m <- 4
CAB_4st <- data.frame(mean = c(41.11, 34.53, 16.86, 40.36, 4.82,
                               35.58, 50.03, 26.18, 40.68, 46.26,
                               29.99, 61.09, 41.76, 41.01, 54.74,
                               17.79, 69.45, 48.91, 9.41, 62.53),
                      state = factor(c(rep(1:m, each = n_dep)), levels = 1:m),
                      CAB_factor = factor(rep(var_names, m), levels = var_names))

m <- 5
CAB_5st <- data.frame(mean = c(41.37, 36.45, 19.48, 39.94, 4.26,
                               36.60, 47.14, 15.18, 39.66, 42.57, 
                               31.27, 59.03, 43.19, 40.55, 52.14,
                               31.83, 58.65, 42.97, 39.67, 53.42,
                               17.41, 69.79, 51.21, 9.45, 63.42),
                      state = factor(c(rep(1:m, each = n_dep)), levels = 1:m),
                      CAB_factor = factor(rep(var_names, m), levels = var_names))

bar_2st <- ggplot(CAB_2st, aes(x = state, y = mean, fill = CAB_factor)) +
  geom_bar(stat="identity") +
  facet_grid(cols = vars(CAB_factor)) + 
  theme_minimal() +
  theme(legend.position="none") +
  xlab("CAB factor") +
  ylim(0,75) +
  ggtitle("2 state model")

bar_3st <- ggplot(CAB_3st, aes(x = state, y = mean, fill = CAB_factor)) +
  geom_bar(stat="identity") +
  facet_grid(cols = vars(CAB_factor)) + 
  theme_minimal() +
  theme(legend.position="none") +
  xlab("CAB factor") +
  ylim(0,75) +
  ggtitle("3 state model")

bar_4st <- ggplot(CAB_4st, aes(x = state, y = mean, fill = CAB_factor)) +
  geom_bar(stat="identity") +
  facet_grid(cols = vars(CAB_factor)) + 
  theme_minimal() +
  theme(legend.position="none") +
  xlab("CAB factor") +
  ylim(0,75) +
  ggtitle("4 state model")

bar_5st <- ggplot(CAB_5st, aes(x = state, y = mean, fill = CAB_factor)) +
  geom_bar(stat="identity") +
  facet_grid(cols = vars(CAB_factor)) + 
  theme_minimal() +
  theme(legend.position="none") +
  xlab("CAB factor") +
  ylim(0,75) +
  ggtitle("5 state model")


plot_grid(bar_2st, bar_3st, bar_4st, bar_5st)


AIC <- data.frame(AIC = c(2663.50, 2632.81, 2621.72, 2633.38), 
                  states = c(2:5))
ggplot(AIC, mapping = aes(x = states, y = AIC)) +
  geom_point() + 
  geom_line() +
  ggtitle("AIC over the 2 - 5 state models") + 
  xlab("Number of states") +
  theme_minimal()
