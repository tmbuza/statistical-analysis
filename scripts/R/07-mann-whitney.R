# Run from repository root: Rscript scripts/R/07-mann-whitney.R
# Base R only. Exact test is selected for this small, untied example.
dat <- read.csv("data/example/07-completion-times.csv", stringsAsFactors = FALSE)
stopifnot(all(c("participant_id", "group", "minutes") %in% names(dat)))
stopifnot(!anyNA(dat), !anyDuplicated(dat$participant_id))
stopifnot(all(nzchar(trimws(dat$participant_id))))
stopifnot(setequal(unique(dat$group), c("A", "B")))
stopifnot(is.numeric(dat$minutes), all(is.finite(dat$minutes)))
stopifnot(all(dat$minutes > 0))
a <- dat$minutes[dat$group == "A"]
b <- dat$minutes[dat$group == "B"]
stopifnot(length(a) >= 2, length(b) >= 2)
# Keep the exact-test assumptions explicit; investigate ties rather than ignore warnings.
stopifnot(!anyDuplicated(c(a, b)))
fit <- wilcox.test(a, b, paired = FALSE, alternative = "two.sided",
                   exact = TRUE, correct = FALSE, conf.int = FALSE)

# R labels the shifted rank-sum statistic W; this equals U_A here.
dat$pooled_rank <- rank(dat$minutes, ties.method = "average")
rank_sum_a <- sum(dat$pooled_rank[dat$group == "A"])
u_a <- rank_sum_a - length(a) * (length(a) + 1) / 2
pairwise_scores <- outer(a, b, function(x, y) (x > y) + 0.5 * (x == y))
probability_a_higher <- mean(pairwise_scores)
stopifnot(isTRUE(all.equal(u_a, unname(fit$statistic))))
stopifnot(isTRUE(all.equal(u_a / (length(a) * length(b)), probability_a_higher)))
result <- data.frame(
  n_A = length(a), n_B = length(b), median_A = median(a), median_B = median(b),
  rank_sum_A = rank_sum_a, U_A = u_a, U_B = length(a) * length(b) - u_a,
  probability_A_higher = probability_a_higher,
  rank_biserial_A_higher = 2 * probability_a_higher - 1,
  exact_two_sided_p = fit$p.value
)
print(fit)
print(result)
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)
write.csv(result, "results/tables/07-mann-whitney.csv", row.names = FALSE)
write.csv(dat, "results/tables/07-pooled-ranks.csv", row.names = FALSE)

dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)
png("results/figures/07-rank-comparison.png", width = 1400, height = 650, res = 150)
par(mfrow = c(1, 2))
stripchart(list(A = a, B = b), method = "stack", vertical = TRUE, pch = 19,
           col = c("#087f8c", "#b86e13"), ylab = "Completion time (minutes)",
           xlab = "Group", main = "Individual completion times")
plot(ecdf(a), xlim = range(c(a, b)), col = "#087f8c", verticals = TRUE,
     xlab = "Completion time (minutes)", ylab = "Cumulative proportion",
     main = "Empirical cumulative distributions")
lines(ecdf(b), col = "#b86e13", verticals = TRUE)
legend("bottomright", legend = c("A", "B"), col = c("#087f8c", "#b86e13"),
       lty = 1, bty = "n")
invisible(dev.off())
