# Run from repository root: Rscript scripts/R/09-one-way-anova.R
# Base R only. Welch is primary; classical ANOVA is a teaching comparison.
dat <- read.csv("data/example/09-three-group-scores.csv", stringsAsFactors = FALSE)
stopifnot(all(c("learner_id", "approach", "score") %in% names(dat)))
stopifnot(!anyNA(dat), !anyDuplicated(dat$learner_id))
stopifnot(all(nzchar(trimws(dat$learner_id))))
stopifnot(setequal(unique(dat$approach), c("A", "B", "C")))
stopifnot(is.numeric(dat$score), all(is.finite(dat$score)))
stopifnot(all(dat$score >= 0 & dat$score <= 100))
dat$approach <- factor(dat$approach, levels = c("A", "B", "C"))
groups <- split(dat$score, dat$approach)
stopifnot(all(lengths(groups) >= 2), all(vapply(groups, sd, numeric(1)) > 0))
welch <- oneway.test(score ~ approach, data = dat, var.equal = FALSE)
classical <- aov(score ~ approach, data = dat)
print(welch)
print(summary(classical))
summary_table <- do.call(rbind, lapply(names(groups), function(g) {
  x <- groups[[g]]
  data.frame(approach = g, n = length(x), mean = mean(x), sd = sd(x))
}))

# Prespecified family: all three two-sided pairwise Welch tests, Holm-adjusted.
pairs <- combn(names(groups), 2, simplify = FALSE)
comparisons <- do.call(rbind, lapply(pairs, function(pair) {
  x <- groups[[pair[1]]]; y <- groups[[pair[2]]]
  fit <- t.test(x, y, var.equal = FALSE, alternative = "two.sided")
  data.frame(comparison = paste(pair, collapse = " minus "),
             difference = mean(x) - mean(y),
             unadjusted_ci_lower = unname(fit$conf.int[1]),
             unadjusted_ci_upper = unname(fit$conf.int[2]),
             t = unname(fit$statistic), df = unname(fit$parameter),
             p_unadjusted = fit$p.value)
}))
comparisons$p_holm <- p.adjust(comparisons$p_unadjusted, method = "holm")
# Classical equal-variance follow-up is separate, for teaching only.
tukey <- as.data.frame(TukeyHSD(classical, "approach")$approach)
tukey$comparison <- rownames(tukey)
print(summary_table)
print(comparisons)
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)
write.csv(summary_table, "results/tables/09-group-summaries.csv", row.names = FALSE)
write.csv(comparisons, "results/tables/09-pairwise-welch-holm.csv", row.names = FALSE)
write.csv(tukey, "results/tables/09-classical-tukey-teaching.csv", row.names = FALSE)
write.csv(data.frame(F = unname(welch$statistic),
                     df_numerator = unname(welch$parameter[1]),
                     df_denominator = unname(welch$parameter[2]), p_value = welch$p.value),
          "results/tables/09-welch-anova.csv", row.names = FALSE)
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)
png("results/figures/09-anova-diagnostics.png", width = 1800, height = 650, res = 150)
par(mfrow = c(1, 3))
stripchart(groups, method = "stack", vertical = TRUE, pch = 19,
           col = c("#087f8c", "#b86e13", "#594b9a"), main = "Observed scores",
           xlab = "Approach", ylab = "Score (points)")
plot(fitted(classical), residuals(classical), pch = 19, col = "#087f8c",
     main = "Residuals versus fitted", xlab = "Fitted group mean", ylab = "Residual")
abline(h = 0, lty = 2)
qqnorm(residuals(classical), pch = 19, col = "#087f8c", main = "Pooled residual Q-Q")
qqline(residuals(classical))
invisible(dev.off())
