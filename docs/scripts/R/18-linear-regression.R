# Run from repository root: Rscript scripts/R/18-linear-regression.R
# Base R only. Simple observational association in synthetic data.
dat <- read.csv("data/example/18-study-scores.csv", stringsAsFactors = FALSE)
stopifnot(all(c("learner_id", "study_hours", "score") %in% names(dat)))
stopifnot(!anyNA(dat), !anyDuplicated(dat$learner_id))
stopifnot(all(nzchar(trimws(dat$learner_id))))
stopifnot(is.numeric(dat$study_hours), is.numeric(dat$score))
stopifnot(all(is.finite(dat$study_hours)), all(is.finite(dat$score)))
stopifnot(all(dat$study_hours >= 0), all(dat$score >= 0 & dat$score <= 100))
stopifnot(nrow(dat) > 3, sd(dat$study_hours) > 0)
fit <- lm(score ~ study_hours, data = dat)
print(summary(fit)); print(confint(fit))
coef_table <- data.frame(term = rownames(coef(summary(fit))),
                         coef(summary(fit)), confint(fit), row.names = NULL,
                         check.names = FALSE)
grid <- data.frame(study_hours = seq(min(dat$study_hours), max(dat$study_hours), length.out = 100))
mean_interval <- predict(fit, newdata = grid, interval = "confidence")
individual_interval <- predict(fit, newdata = grid, interval = "prediction")
predictions <- data.frame(grid, fitted = mean_interval[, "fit"],
                           mean_ci_lower = mean_interval[, "lwr"],
                           mean_ci_upper = mean_interval[, "upr"],
                           prediction_lower = individual_interval[, "lwr"],
                           prediction_upper = individual_interval[, "upr"])
diagnostics <- data.frame(learner_id = dat$learner_id, fitted = fitted(fit),
                           residual = residuals(fit), leverage = hatvalues(fit),
                           cooks_distance = cooks.distance(fit))
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)
write.csv(coef_table, "results/tables/18-coefficients.csv", row.names = FALSE)
write.csv(predictions, "results/tables/18-predictions.csv", row.names = FALSE)
write.csv(diagnostics, "results/tables/18-diagnostics.csv", row.names = FALSE)
write.csv(data.frame(n = nrow(dat), residual_df = df.residual(fit),
                     residual_sd = summary(fit)$sigma, R_squared = summary(fit)$r.squared),
          "results/tables/18-model-summary.csv", row.names = FALSE)
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)
png("results/figures/18-regression-intervals.png", width = 1300, height = 850, res = 150)
plot(dat$study_hours, dat$score, type = "n", ylim = range(c(dat$score, individual_interval)),
     xlab = "Study time (hours in preceding week)", ylab = "Assessment score (points)",
     main = "Fitted association: mean confidence and individual prediction intervals")
lines(grid$study_hours, individual_interval[, "lwr"], lty = 2, col = "#b86e13")
lines(grid$study_hours, individual_interval[, "upr"], lty = 2, col = "#b86e13")
polygon(c(grid$study_hours, rev(grid$study_hours)),
        c(mean_interval[, "lwr"], rev(mean_interval[, "upr"])),
        col = "#d3eeee", border = NA)
lines(grid$study_hours, mean_interval[, "fit"], col = "#087f8c", lwd = 2)
points(dat$study_hours, dat$score, pch = 19)
legend("topleft", legend = c("Fitted mean", "Pointwise mean 95% CI", "Individual 95% PI"),
       col = c("#087f8c", "#d3eeee", "#b86e13"), lty = c(1, 1, 2), lwd = c(2, 6, 1), bty = "n")
invisible(dev.off())
png("results/figures/18-regression-diagnostics.png", width = 1500, height = 1200, res = 150)
par(mfrow = c(2, 2))
plot(fit, which = c(1, 2, 3, 5))
invisible(dev.off())
