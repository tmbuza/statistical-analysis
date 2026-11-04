# Run from repository root:
# Rscript scripts/R/08-wilcoxon-signed-rank.R
# Base R only. Small synthetic example without zero differences or tied magnitudes.
dat <- read.csv("data/example/08-paired-scores.csv", stringsAsFactors = FALSE)
stopifnot(all(c("learner_id", "before", "after") %in% names(dat)))
stopifnot(!anyNA(dat), !anyDuplicated(dat$learner_id))
stopifnot(all(nzchar(trimws(dat$learner_id))))
stopifnot(is.numeric(dat$before), is.numeric(dat$after))
stopifnot(all(is.finite(dat$before)), all(is.finite(dat$after)))
stopifnot(all(dat$before >= 0 & dat$before <= 100))
stopifnot(all(dat$after >= 0 & dat$after <= 100))
dat$change <- dat$after - dat$before
stopifnot(nrow(dat) >= 2)
# This guard preserves the documented untied exact example.
stopifnot(all(dat$change != 0), !anyDuplicated(abs(dat$change)))
dat$absolute_rank <- rank(abs(dat$change))
v_positive <- sum(dat$absolute_rank[dat$change > 0])
v_negative <- sum(dat$absolute_rank[dat$change < 0])
fit <- wilcox.test(dat$after, dat$before, paired = TRUE,
                   alternative = "two.sided", exact = TRUE,
                   correct = FALSE, conf.int = FALSE)
stopifnot(isTRUE(all.equal(unname(fit$statistic), v_positive)))
# Walsh averages: all (d_i + d_j)/2 with i <= j.
d <- dat$change
walsh_matrix <- outer(d, d, "+") / 2
walsh <- walsh_matrix[upper.tri(walsh_matrix, diag = TRUE)]
result <- data.frame(
  complete_pairs = nrow(dat), nonzero_pairs = sum(d != 0),
  median_change = median(d), pseudomedian_estimate = median(walsh),
  V_positive = v_positive, V_negative = v_negative,
  matched_rank_biserial = (v_positive - v_negative) / (v_positive + v_negative),
  exact_two_sided_p = fit$p.value
)
print(fit)
print(result)
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)
write.csv(result, "results/tables/08-signed-rank.csv", row.names = FALSE)
write.csv(dat, "results/tables/08-signed-ranks.csv", row.names = FALSE)
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)
png("results/figures/08-change-diagnostics.png", width = 1400, height = 650, res = 150)
par(mfrow = c(1, 2))
stripchart(d, method = "stack", vertical = TRUE, pch = 19,
           col = "#087f8c", main = "Observed paired changes",
           ylab = "After minus before (points)", xlab = "Learners")
abline(h = 0, lty = 2)
# Reflection around the sample median is a visual aid, not a symmetry test.
centered <- d - median(d)
plot(ecdf(centered), xlim = range(c(centered, -centered)), col = "#087f8c",
     main = "Visual symmetry check", xlab = "Change minus sample median",
     ylab = "Cumulative proportion")
lines(ecdf(-centered), col = "#b86e13")
legend("bottomright", legend = c("Centered changes", "Reflected changes"),
       lty = 1, col = c("#087f8c", "#b86e13"), bty = "n")
invisible(dev.off())
