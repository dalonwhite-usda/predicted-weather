# Approximation before the actual distribution is available from C. Fautt.
adult_survival_distro <- function(
    temp,
    cold_reference_temp = 5 * (9/5) + 32,
    cold_reference_survival = 0.725,
    plateau_start_temp = 16 * (9/5) + 32,
    optimal_temp = 27.2 * (9/5) + 32,
    optimal_survival = 0.94,
    hot_reference_temp = 45 * (9/5) + 32,
    hot_reference_survival = 0.825) {
    if (!is.numeric(temp)) {
        stop("`temp` must be a numeric vector, matrix, or array.", call. = FALSE)
    }
    if (any(!is.finite(temp) & !is.na(temp))) {
        stop("`temp` must contain finite values or NA.", call. = FALSE)
    }

    parameters <- list(
        cold_reference_temp = cold_reference_temp,
        cold_reference_survival = cold_reference_survival,
        plateau_start_temp = plateau_start_temp,
        optimal_temp = optimal_temp,
        optimal_survival = optimal_survival,
        hot_reference_temp = hot_reference_temp,
        hot_reference_survival = hot_reference_survival
    )
    valid_parameters <- vapply(parameters, function(value) {
        is.numeric(value) && length(value) == 1L && is.finite(value)
    }, logical(1))
    if (!all(valid_parameters)) {
        stop("All model parameters must be finite numeric scalars.", call. = FALSE)
    }
    if (!(cold_reference_temp < plateau_start_temp &&
          plateau_start_temp < optimal_temp && optimal_temp < hot_reference_temp)) {
        stop("Model temperatures must satisfy cold reference < plateau start < optimum < hot reference.",
             call. = FALSE)
    }
    if (!(0 <= cold_reference_survival &&
          cold_reference_survival <= optimal_survival &&
          0 < hot_reference_survival &&
          hot_reference_survival < optimal_survival &&
          optimal_survival <= 1)) {
        stop("Survival parameters must be probabilities with lower and upper references below the optimum.",
             call. = FALSE)
    }

    result <- rep(NA_real_, length(temp))
    has_temp <- !is.na(temp)
    cold <- has_temp & temp < plateau_start_temp
    plateau <- has_temp & temp >= plateau_start_temp & temp <= optimal_temp
    hot <- has_temp & temp > optimal_temp

    cold_slope <- (optimal_survival - cold_reference_survival) /
        (plateau_start_temp - cold_reference_temp)
    result[cold] <- cold_reference_survival +
        cold_slope * (temp[cold] - cold_reference_temp)
    result[plateau] <- optimal_survival

    decay_rate <- log(optimal_survival / hot_reference_survival) /
        (hot_reference_temp - optimal_temp)
    result[hot] <- optimal_survival *
        exp(-decay_rate * (temp[hot] - optimal_temp))

    if (!is.null(dim(temp))) {
        dim(result) <- dim(temp)
        dimnames(result) <- dimnames(temp)
    }
    result
}