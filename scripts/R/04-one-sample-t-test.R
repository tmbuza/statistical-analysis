# Run from the repository root:
# Rscript scripts/R/04-one-sample-t-test.R
# Base R only. Input is synthetic and supplied separately.

input_path <- "data/example/04-assessment-scores.csv"
dat <- read.csv(input_path, stringsAsFactors = FALSE)
stopifnot(all(c("learner_id", "score") %in% names(dat)))
stopifnot(!anyNA(dat$learner_id), all(nzchar(trimws(dat$learner_id))))
stopifnot(!anyDuplicated(dat$learner_id), is.numeric(dat$score))

# Stop for missing scores: investigate rather than silently omit them.
stopifnot(!anyNA(dat$score), all(is.finite(dat$score)))
stopifnot(all(dat$score >= 0 & dat$score <= 100))
scores <- dat$score
stopifnot(length(scores) >= 2, sd(scores) > 0)
reference_mean <- 65

# Estimate the population mean and its difference from the fixed reference.
fit <- t.test(scores, mu = reference_mean, alternative = "two.sided",
              conf.level = 0.95)
summary_result <- data.frame(
  n = length(scores),
  reference_mean = reference_mean,
  mean = mean(scores),
  sd = sd(scores),
  standard_error = sd(scores) / sqrt(length(scores)),
  difference = mean(scores) - reference_mean,
  mean_ci_lower = unname(fit$conf.int[1]),
  mean_ci_upper = unname(fit$conf.int[2]),
  difference_ci_lower = unname(fit$conf.int[1]) - reference_mean,
  difference_ci_upper = unname(fit$conf.int[2]) - reference_mean,
  t = unname(fit$statistic),
  df = unname(fit$parameter),
  p_value = fit$p.value,
  cohens_d = (mean(scores) - reference_mean) / sd(scores)
)
print(fit)
print(summary_result)
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)
write.csv(summary_result, "results/tables/04-one-sample-t-test.csv",
          row.names = FALSE)

# Save diagnostic plots separately from the book rendering process.
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)
png("results/figures/04-one-sample-diagnostics.png",
    width = 1400, height = 650, res = 150)
par(mfrow = c(1, 2))
stripchart(scores, method = "stack", vertical = TRUE, pch = 19,
           col = "#087f8c", ylab = "Assessment score (points)",
           main = "Observed scores", xlab = "Synthetic learners")
abline(h = reference_mean, lty = 2, col = "#b86e13")
legend("topleft", legend = "Reference mean: 65", lty = 2,
       col = "#b86e13", bty = "n")
qqnorm(scores, pch = 19, col = "#087f8c", main = "Normal Q-Q plot")
qqline(scores, col = "#243746")
invisible(dev.off())
