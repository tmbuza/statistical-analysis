# Run from repository root: Rscript scripts/R/06-paired-t-test.R
# Uses base R and the supplied synthetic data. One row per matched learner.
dat <- read.csv("data/example/06-paired-scores.csv", stringsAsFactors = FALSE)
stopifnot(all(c("learner_id", "before", "after") %in% names(dat)))
stopifnot(!anyNA(dat$learner_id), !anyDuplicated(dat$learner_id))
stopifnot(all(nzchar(trimws(dat$learner_id))))
stopifnot(is.numeric(dat$before), is.numeric(dat$after))

# Stop for incomplete pairs; investigate instead of silently deleting records.
stopifnot(!anyNA(dat$before), !anyNA(dat$after))
stopifnot(all(is.finite(dat$before)), all(is.finite(dat$after)))
stopifnot(all(dat$before >= 0 & dat$before <= 100))
stopifnot(all(dat$after >= 0 & dat$after <= 100))
dat$change <- dat$after - dat$before
stopifnot(nrow(dat) >= 2, sd(dat$change) > 0)

# Explicit order defines change as after minus before.
fit <- t.test(dat$after, dat$before, paired = TRUE,
              alternative = "two.sided", conf.level = 0.95)
check <- t.test(dat$change, mu = 0, alternative = "two.sided",
                conf.level = 0.95)
stopifnot(isTRUE(all.equal(unname(fit$statistic), unname(check$statistic))))
stopifnot(isTRUE(all.equal(fit$p.value, check$p.value)))
result <- data.frame(
  pairs = nrow(dat), mean_before = mean(dat$before),
  mean_after = mean(dat$after), mean_change = mean(dat$change),
  sd_change = sd(dat$change), se_change = sd(dat$change) / sqrt(nrow(dat)),
  ci_lower = unname(fit$conf.int[1]), ci_upper = unname(fit$conf.int[2]),
  t = unname(fit$statistic), df = unname(fit$parameter), p_value = fit$p.value
)
print(fit)
print(result)
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)
write.csv(result, "results/tables/06-paired-t-test.csv", row.names = FALSE)
write.csv(dat, "results/tables/06-paired-changes.csv", row.names = FALSE)

# Save plots outside the Quarto rendering process.
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)
png("results/figures/06-paired-diagnostics.png", width = 1800,
    height = 650, res = 150)
par(mfrow = c(1, 3))
plot(c(1, 2), range(c(dat$before, dat$after)), type = "n", xaxt = "n",
     xlab = "Assessment", ylab = "Score (points)", main = "Matched learners")
axis(1, at = c(1, 2), labels = c("Before", "After"))
for (i in seq_len(nrow(dat))) {
  lines(c(1, 2), c(dat$before[i], dat$after[i]), type = "b", pch = 19,
        col = if (dat$change[i] < 0) "#b86e13" else "#087f8c")
}
stripchart(dat$change, method = "stack", vertical = TRUE, pch = 19,
           col = "#087f8c", main = "Within-learner changes",
           ylab = "After minus before (points)", xlab = "Learners")
abline(h = 0, lty = 2)
qqnorm(dat$change, pch = 19, col = "#087f8c", main = "Normal Q-Q: changes")
qqline(dat$change)
invisible(dev.off())
