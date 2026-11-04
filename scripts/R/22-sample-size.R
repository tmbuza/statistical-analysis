# Run from repository root: Rscript scripts/R/22-sample-size.R
# Base R. Equal allocation, independent two-sample t-test, common SD assumption.
a <- read.csv("data/example/22-planning-assumptions.csv")
stopifnot(nrow(a) == 1, !anyNA(a), all(vapply(a, is.numeric, logical(1))))
stopifnot(all(is.finite(as.matrix(a))), a$meaningful_difference > 0, a$common_sd > 0)
stopifnot(a$target_power > 0 & a$target_power < 1, a$alpha > 0 & a$alpha < 1)
stopifnot(a$expected_loss_fraction >= 0 & a$expected_loss_fraction < 1)
plan <- power.t.test(delta = a$meaningful_difference, sd = a$common_sd,
                     sig.level = a$alpha, power = a$target_power,
                     type = "two.sample", alternative = "two.sided", strict = TRUE)
n_per_group <- ceiling(plan$n)
achieved <- power.t.test(n = n_per_group, delta = a$meaningful_difference,
                         sd = a$common_sd, sig.level = a$alpha,
                         type = "two.sample", alternative = "two.sided", strict = TRUE)$power
recruit_per_group <- ceiling(n_per_group / (1 - a$expected_loss_fraction))
result <- data.frame(a, continuous_n_per_group = plan$n,
                     analyzable_n_per_group = n_per_group,
                     achieved_power = achieved, recruit_n_per_group = recruit_per_group,
                     total_recruit = 2 * recruit_per_group)
# Sensitivity to the unknown SD, with the other planning choices fixed.
sensitivity <- do.call(rbind, lapply(c(8, 10, 12), function(s) {
  out <- power.t.test(delta = a$meaningful_difference, sd = s, power = a$target_power,
                      sig.level = a$alpha, type = "two.sample",
                      alternative = "two.sided", strict = TRUE)
  data.frame(assumed_SD = s, analyzable_n_per_group = ceiling(out$n))
}))
curve <- data.frame(n_per_group = 5:150)
curve$power <- vapply(curve$n_per_group, function(n) {
  power.t.test(n = n, delta = a$meaningful_difference, sd = a$common_sd,
               sig.level = a$alpha, type = "two.sample", alternative = "two.sided",
               strict = TRUE)$power
}, numeric(1))
print(result); print(sensitivity)
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)
write.csv(result, "results/tables/22-sample-size-plan.csv", row.names = FALSE)
write.csv(sensitivity, "results/tables/22-sd-sensitivity.csv", row.names = FALSE)
write.csv(curve, "results/tables/22-power-curve.csv", row.names = FALSE)
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)
png("results/figures/22-power-curve.png", width = 1200, height = 800, res = 150)
plot(curve$n_per_group, curve$power, type = "l", lwd = 2, col = "#087f8c",
     ylim = c(0, 1), xlab = "Analyzable independent participants per group",
     ylab = "Power under specified alternative", main = "Two-sided equal-allocation t-test planning")
abline(h = a$target_power, v = n_per_group, lty = 2, col = "#b86e13")
invisible(dev.off())
