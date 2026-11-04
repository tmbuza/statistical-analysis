# Run from repository root: Rscript scripts/R/10-kruskal-wallis.R
# Base R only. Overall chi-square approximation; separate pairwise rank-sum follow-up.
dat <- read.csv("data/example/10-completion-times.csv", stringsAsFactors = FALSE)
stopifnot(all(c("participant_id", "group", "minutes") %in% names(dat)))
stopifnot(!anyNA(dat), !anyDuplicated(dat$participant_id))
stopifnot(all(nzchar(trimws(dat$participant_id))))
stopifnot(setequal(unique(dat$group), c("A", "B", "C")))
stopifnot(is.numeric(dat$minutes), all(is.finite(dat$minutes)), all(dat$minutes > 0))
dat$group <- factor(dat$group, levels = c("A", "B", "C"))
groups <- split(dat$minutes, dat$group)
stopifnot(all(lengths(groups) >= 2))
dat$pooled_rank <- rank(dat$minutes, ties.method = "average")
fit <- kruskal.test(minutes ~ group, data = dat)
N <- nrow(dat)
rank_sums <- tapply(dat$pooled_rank, dat$group, sum)
sizes <- lengths(groups)
h_raw <- 12 / (N * (N + 1)) * sum(rank_sums^2 / sizes) - 3 * (N + 1)
tie_sizes <- as.numeric(table(dat$minutes))
tie_correction <- 1 - sum(tie_sizes^3 - tie_sizes) / (N^3 - N)
stopifnot(tie_correction > 0)
stopifnot(isTRUE(all.equal(unname(fit$statistic), unname(h_raw / tie_correction))))
summary_table <- do.call(rbind, lapply(names(groups), function(g) {
  x <- groups[[g]]
  data.frame(group = g, n = length(x), median = median(x), IQR = IQR(x),
             rank_sum = unname(rank_sums[g]), mean_rank = unname(rank_sums[g]) / length(x))
}))
# Each follow-up reranks ONLY the two groups being compared (not Dunn's procedure).
pairs <- combn(names(groups), 2, simplify = FALSE)
followup <- do.call(rbind, lapply(pairs, function(pair) {
  x <- groups[[pair[1]]]; y <- groups[[pair[2]]]
  untied <- !anyDuplicated(c(x, y))
  test <- wilcox.test(x, y, paired = FALSE, alternative = "two.sided",
                      exact = untied, correct = FALSE, conf.int = FALSE)
  data.frame(comparison = paste(pair, collapse = " versus "),
             U_first_group = unname(test$statistic),
             method = if (untied) "exact" else "tie-adjusted normal, no continuity correction",
             p_unadjusted = test$p.value)
}))
followup$p_holm <- p.adjust(followup$p_unadjusted, method = "holm")
print(fit); print(summary_table); print(followup)
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)
write.csv(summary_table, "results/tables/10-group-ranks.csv", row.names = FALSE)
write.csv(dat, "results/tables/10-pooled-ranks.csv", row.names = FALSE)
write.csv(followup, "results/tables/10-pairwise-rank-sum-holm.csv", row.names = FALSE)
write.csv(data.frame(H = unname(fit$statistic), df = unname(fit$parameter),
                     p_value = fit$p.value, tie_correction = tie_correction),
          "results/tables/10-kruskal-wallis.csv", row.names = FALSE)
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)
png("results/figures/10-group-distributions.png", width = 1400, height = 650, res = 150)
par(mfrow = c(1, 2))
colors <- c("#087f8c", "#b86e13", "#594b9a")
stripchart(groups, method = "stack", vertical = TRUE, pch = 19, col = colors,
           xlab = "Group", ylab = "Completion time (minutes)", main = "Observed times")
plot(ecdf(groups[[1]]), xlim = range(dat$minutes), col = colors[1],
     xlab = "Completion time (minutes)", ylab = "Cumulative proportion",
     main = "Empirical cumulative distributions")
for (i in 2:3) lines(ecdf(groups[[i]]), col = colors[i])
legend("bottomright", legend = names(groups), col = colors, lty = 1, bty = "n")
invisible(dev.off())
