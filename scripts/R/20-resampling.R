# Run from repository root: Rscript scripts/R/20-resampling.R
# Base R. Separate within-group bootstrap and null label-permutation distributions.
dat <- read.csv("data/example/20-two-group-values.csv", stringsAsFactors = FALSE)
stopifnot(all(c("participant_id", "group", "value") %in% names(dat)))
stopifnot(!anyNA(dat), !anyDuplicated(dat$participant_id))
stopifnot(all(nzchar(trimws(dat$participant_id))))
stopifnot(setequal(dat$group, c("A", "B")), is.numeric(dat$value))
stopifnot(all(is.finite(dat$value)))
a <- dat$value[dat$group == "A"]; b <- dat$value[dat$group == "B"]
stopifnot(length(a) == 6, length(b) == 6)
observed <- mean(a) - mean(b)
B <- 19999L
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
set.seed(202620)
# Approximate sampling uncertainty: resample independently WITHIN each group.
bootstrap <- replicate(B, {
  mean(sample(a, length(a), replace = TRUE)) -
    mean(sample(b, length(b), replace = TRUE))
})
percentile_ci <- quantile(bootstrap, c(0.025, 0.975), type = 7, names = FALSE)
# Exact enumeration: observations remain distinguishable, even when values tie.
pooled <- c(a, b)
allocations <- combn(seq_along(pooled), length(a))
permuted <- apply(allocations, 2, function(index) {
  mean(pooled[index]) - mean(pooled[-index])
})
tol <- 1e-12
extreme_exact <- sum(abs(permuted) >= abs(observed) - tol)
p_exact <- extreme_exact / length(permuted)
# Monte Carlo comparison: independent random label allocations with replacement.
set.seed(202621)
permuted_mc <- replicate(B, {
  index <- sample.int(length(pooled), length(a), replace = FALSE)
  mean(pooled[index]) - mean(pooled[-index])
})
extreme_mc <- sum(abs(permuted_mc) >= abs(observed) - tol)
p_mc <- (extreme_mc + 1) / (B + 1)
mc_se_approx <- sqrt(p_mc * (1 - p_mc) / B)
result <- data.frame(mean_A = mean(a), mean_B = mean(b), difference_A_minus_B = observed,
                     bootstrap_replicates = B, bootstrap_SE = sd(bootstrap),
                     percentile_lower = percentile_ci[1], percentile_upper = percentile_ci[2],
                     exact_allocations = length(permuted), exact_extreme = extreme_exact,
                     exact_permutation_p = p_exact, monte_carlo_p_plus_one = p_mc,
                     monte_carlo_SE_approx = mc_se_approx)
print(result)
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)
write.csv(result, "results/tables/20-resampling-summary.csv", row.names = FALSE)
write.csv(data.frame(difference = bootstrap), "results/tables/20-bootstrap-draws.csv", row.names = FALSE)
write.csv(data.frame(difference = permuted), "results/tables/20-exact-permutations.csv", row.names = FALSE)
capture.output(sessionInfo(), file = "results/tables/20-session-info.txt")
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)
png("results/figures/20-resampling-distributions.png", width = 1500, height = 750, res = 150)
par(mfrow = c(1, 2))
hist(bootstrap, breaks = 30, col = "#d3eeee", border = "white",
     main = "Bootstrap: sampling uncertainty", xlab = "Mean difference, A minus B")
abline(v = observed, col = "#087f8c", lwd = 2)
abline(v = percentile_ci, col = "#b86e13", lty = 2)
hist(permuted, breaks = 25, col = "#d3eeee", border = "white",
     main = "Permutation: null reference", xlab = "Mean difference, A minus B")
abline(v = c(-abs(observed), abs(observed)), col = "#b86e13", lty = 2)
abline(v = 0, col = "gray50")
invisible(dev.off())
