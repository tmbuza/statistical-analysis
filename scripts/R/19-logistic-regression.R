# Run from repository root: Rscript scripts/R/19-logistic-regression.R
# Base R only. completed=1 is the modeled event; one independent learner per row.
dat <- read.csv("data/example/19-practice-completion.csv", stringsAsFactors = FALSE)
stopifnot(all(c("learner_id", "practice_sessions", "completed") %in% names(dat)))
stopifnot(!anyNA(dat), !anyDuplicated(dat$learner_id))
stopifnot(all(nzchar(trimws(dat$learner_id))))
stopifnot(is.numeric(dat$practice_sessions), is.numeric(dat$completed))
stopifnot(all(is.finite(dat$practice_sessions)), all(dat$practice_sessions >= 0))
stopifnot(setequal(unique(dat$completed), c(0, 1)))
fit <- glm(completed ~ practice_sessions, data = dat, family = binomial(link = "logit"))
stopifnot(fit$converged, all(is.finite(coef(fit))))
# Convergence checks alone do not rule out separation or unstable estimates.
ct <- coef(summary(fit))
wald <- confint.default(fit, level = 0.95)
coef_table <- data.frame(term = rownames(ct), log_odds_coefficient = ct[, 1],
                         standard_error = ct[, 2], z = ct[, 3], p_wald = ct[, 4],
                         odds_ratio = exp(ct[, 1]), OR_lower_wald = exp(wald[, 1]),
                         OR_upper_wald = exp(wald[, 2]), row.names = NULL)
grid <- data.frame(practice_sessions = seq(min(dat$practice_sessions),
                                            max(dat$practice_sessions), length.out = 100))
pr <- predict(fit, newdata = grid, type = "link", se.fit = TRUE)
zcrit <- qnorm(0.975)
probabilities <- data.frame(grid, probability = plogis(pr$fit),
                             lower_pointwise_wald = plogis(pr$fit - zcrit * pr$se.fit),
                             upper_pointwise_wald = plogis(pr$fit + zcrit * pr$se.fit))
observed <- aggregate(completed ~ practice_sessions, data = dat, FUN = mean)
print(summary(fit)); print(coef_table)
print(predict(fit, newdata = data.frame(practice_sessions = 4), type = "response"))
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)
write.csv(coef_table, "results/tables/19-logistic-coefficients.csv", row.names = FALSE)
write.csv(probabilities, "results/tables/19-predicted-probabilities.csv", row.names = FALSE)
write.csv(data.frame(learner_id = dat$learner_id, observed = dat$completed,
                     fitted_probability = fitted(fit), deviance_residual = residuals(fit, type = "deviance"),
                     leverage = hatvalues(fit), cooks_distance = cooks.distance(fit)),
          "results/tables/19-observation-diagnostics.csv", row.names = FALSE)
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)
png("results/figures/19-probability-curve.png", width = 1200, height = 800, res = 150)
plot(grid$practice_sessions, probabilities$probability, type = "n", ylim = c(0, 1),
     xlab = "Practice sessions", ylab = "Probability of completion",
     main = "Synthetic completion: fitted probabilities and pointwise 95% intervals")
polygon(c(grid$practice_sessions, rev(grid$practice_sessions)),
        c(probabilities$lower_pointwise_wald, rev(probabilities$upper_pointwise_wald)),
        col = "#d3eeee", border = NA)
lines(grid$practice_sessions, probabilities$probability, col = "#087f8c", lwd = 2)
points(observed$practice_sessions, observed$completed, pch = 19, col = "#b86e13")
legend("topleft", legend = c("Fitted probability", "Observed proportion per session count"),
       col = c("#087f8c", "#b86e13"), lty = c(1, NA), pch = c(NA, 19), bty = "n")
invisible(dev.off())
