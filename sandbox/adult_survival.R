# Model adult survival distribution
# Based on Chad Fautt's modeling (ARS)
    # Adams 1979, constant temperature tests
    # Matlock and Skoda 2009, field females survival tests

# Keep the exploratory script and operational pipeline on the same model.
source("R/adult_survival.R")

temp <- seq(10, 120, 10)
survival <- sapply(temp, adult_survival_distro)
test_survival <- data.frame(temp = temp, survival = survival)
test_survival
