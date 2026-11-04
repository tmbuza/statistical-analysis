# Run from repository root: Rscript scripts/R/16-pearson.R
# Base R only. One independent learner per row; both measurements stay paired.
dat <- read.csv("data/example/16-study-scores.csv", stringsAsFactors = FALSE)
stopifnot(all(c("learner_id", "study_hours", "score") %in% names(dat)))
stopifnot(!anyNA(dat), !anyDuplicated(dat$learner_id))
stopifnot(all(nzchar(trimws(dat$learner_id))))
stopifnot(is.numeric(dat$study_hours), is.numeric(dat$score))
stopifnot(all(is.finite(dat$study_hours)), all(is.finite(dat$score)))
stopifnot(all(dat$study_hours >= 0), all(dat$score >= 0 & dat$score <= 100))
stopifnot(nrow(dat) > 3, sd(dat$study_hours) > 0, sd(dat$score) > 0)
fit <- cor.test(dat$study_hours, dat$score, method = "pearson",
                alternative = "two.sided", conf.level = 0.95)
result <- data.frame(n = nrow(dat), r = unname(fit$estimate),
                     ci_lower_fisher_z = fit$conf.int[1],
                     ci_upper_fisher_z = fit$conf.int[2],
                     t = unname(fit$statistic), df = unname(fit$parameter),
                     p_value = fit$p.value)
# Leave-one-out correlations are descriptive sensitivity checks, not selection rules.
loo <- data.frame(omitted_id = dat$learner_id,
                  r_without_learner = vapply(seq_len(nrow(dat)), function(i) {
                    cor(dat$study_hours[-i], dat$score[-i])
                  }, numeric(1)))
print(fit); print(result); print(range(loo$r_without_learner))
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)
write.csv(result, "results/tables/16-pearson.csv", row.names = FALSE)
write.csv(loo, "results/tables/16-leave-one-out.csv", row.names = FALSE)
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)
png("results/figures/16-study-score-scatter.png", width = 1100, height = 750, res = 150)
plot(dat$study_hours, dat$score, pch = 19, col = "#087f8c",
     xlab = "Study time (hours in preceding week)", ylab = "Assessment score (points)",
     main = "Synthetic study time and assessment scores")
abline(lm(score ~ study_hours, data = dat), col = "#b86e13", lwd = 2)
invisible(dev.off())
