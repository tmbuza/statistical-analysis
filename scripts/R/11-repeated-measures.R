# Run from repository root: Rscript scripts/R/11-repeated-measures.R
# Base R only. Complete balanced one-factor design; GG-corrected ANOVA is primary.
dat <- read.csv("data/example/11-repeated-scores.csv", stringsAsFactors = FALSE)
conditions <- c("baseline", "week4", "week8")
stopifnot(all(c("learner_id", conditions) %in% names(dat)))
stopifnot(!anyNA(dat), !anyDuplicated(dat$learner_id))
stopifnot(all(nzchar(trimws(dat$learner_id))))
stopifnot(all(vapply(dat[conditions], is.numeric, logical(1))))
x <- as.matrix(dat[conditions])
stopifnot(all(is.finite(x)), all(x >= 0 & x <= 100), nrow(x) >= 3)
n <- nrow(x); k <- ncol(x)
long <- reshape(dat, varying = conditions, v.names = "score", timevar = "condition",
                times = conditions, idvar = "learner_id", direction = "long")
long$condition <- factor(long$condition, levels = conditions)
long$learner_id <- factor(long$learner_id)
fit <- aov(score ~ condition + Error(learner_id/condition), data = long)
print(summary(fit))

# Verify the classical balanced-design decomposition directly.
grand_mean <- mean(x)
ss_condition <- n * sum((colMeans(x) - grand_mean)^2)
residual_matrix <- sweep(sweep(x, 1, rowMeans(x), "-"), 2,
                         colMeans(x) - grand_mean, "-")
ss_error <- sum(residual_matrix^2)
df1 <- k - 1; df2 <- (n - 1) * (k - 1)
stopifnot(ss_error > 0)
f_stat <- (ss_condition / df1) / (ss_error / df2)
# Greenhouse-Geisser epsilon from the centered sample covariance matrix.
P <- diag(k) - matrix(1 / k, k, k)
V <- P %*% cov(x) %*% P
stopifnot(sum(V * V) > 0)
epsilon <- sum(diag(V))^2 / ((k - 1) * sum(V * V))
epsilon <- max(1 / (k - 1), min(1, epsilon))
anova_table <- data.frame(F = f_stat, df1 = df1, df2 = df2,
                          p_uncorrected = pf(f_stat, df1, df2, lower.tail = FALSE),
                          epsilon_GG = epsilon, df1_GG = epsilon * df1,
                          df2_GG = epsilon * df2,
                          p_GG = pf(f_stat, epsilon * df1, epsilon * df2,
                                    lower.tail = FALSE))
fr <- friedman.test(x)
ranks <- t(apply(x, 1, rank, ties.method = "average"))
friedman_table <- data.frame(chi_square = unname(fr$statistic),
                             df = unname(fr$parameter), p_approximate = fr$p.value,
                             kendalls_W = unname(fr$statistic) / (n * (k - 1)))
# Primary mean-based follow-up: all three paired t-tests with Holm adjustment.
pairs <- combn(conditions, 2, simplify = FALSE)
followup <- do.call(rbind, lapply(pairs, function(pair) {
  difference <- dat[[pair[2]]] - dat[[pair[1]]]
  test <- t.test(difference, mu = 0, alternative = "two.sided")
  data.frame(comparison = paste(pair[2], "minus", pair[1]),
             mean_change = mean(difference), unadjusted_ci_lower = test$conf.int[1],
             unadjusted_ci_upper = test$conf.int[2], p_unadjusted = test$p.value)
}))
followup$p_holm <- p.adjust(followup$p_unadjusted, method = "holm")
print(anova_table); print(fr); print(followup)
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)
write.csv(anova_table, "results/tables/11-repeated-anova.csv", row.names = FALSE)
write.csv(friedman_table, "results/tables/11-friedman.csv", row.names = FALSE)
write.csv(data.frame(condition = conditions, mean = colMeans(x),
                     sd = apply(x, 2, sd), mean_within_learner_rank = colMeans(ranks)),
          "results/tables/11-condition-summaries.csv", row.names = FALSE)
write.csv(followup, "results/tables/11-paired-t-holm.csv", row.names = FALSE)
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)
png("results/figures/11-trajectories.png", width = 1200, height = 750, res = 150)
matplot(seq_len(k), t(x), type = "b", pch = 19, lty = 1, col = "gray65",
        xaxt = "n", xlab = "Assessment", ylab = "Score (points)",
        main = "Within-learner scores and condition means")
axis(1, at = seq_len(k), labels = conditions)
lines(seq_len(k), colMeans(x), lwd = 3, col = "#087f8c", type = "b", pch = 19)
invisible(dev.off())
