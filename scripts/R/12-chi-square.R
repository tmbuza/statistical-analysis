# Run from repository root: Rscript scripts/R/12-chi-square.R
# Base R only. Aggregated counts represent independent synthetic participants.
dat <- read.csv("data/example/12-learning-preferences.csv", stringsAsFactors = FALSE)
stopifnot(all(c("experience", "preference", "count") %in% names(dat)))
stopifnot(!anyNA(dat), !anyDuplicated(dat[c("experience", "preference")]))
stopifnot(setequal(unique(dat$experience), c("Beginner", "Intermediate")))
stopifnot(setequal(unique(dat$preference), c("Video", "Reading", "Practice")))
stopifnot(nrow(dat) == 6, is.numeric(dat$count), all(is.finite(dat$count)))
stopifnot(all(dat$count >= 0), all(dat$count == floor(dat$count)))
dat$experience <- factor(dat$experience, levels = c("Beginner", "Intermediate"))
dat$preference <- factor(dat$preference, levels = c("Video", "Reading", "Practice"))
tab <- xtabs(count ~ experience + preference, data = dat)
stopifnot(all(rowSums(tab) > 0), all(colSums(tab) > 0))
fit <- chisq.test(tab, correct = FALSE)
expected <- outer(rowSums(tab), colSums(tab)) / sum(tab)
contribution <- (tab - expected)^2 / expected
stopifnot(isTRUE(all.equal(unname(sum(contribution)), unname(fit$statistic))))
cramers_v <- sqrt(unname(fit$statistic) /
                   (sum(tab) * min(nrow(tab) - 1, ncol(tab) - 1)))
result <- data.frame(N = sum(tab), X_squared = unname(fit$statistic),
                     df = unname(fit$parameter), p_value = fit$p.value,
                     cramers_V = cramers_v, minimum_expected = min(expected),
                     fraction_expected_below_5 = mean(expected < 5))
print(tab); print(fit); print(expected); print(result)
# Export matrices in long form to preserve category names and labels.
long_matrix <- function(m, name) {
  out <- as.data.frame(as.table(m))
  names(out) <- c("experience", "preference", name)
  out
}
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)
write.csv(result, "results/tables/12-chi-square.csv", row.names = FALSE)
write.csv(long_matrix(expected, "expected"), "results/tables/12-expected-counts.csv",
          row.names = FALSE)
write.csv(long_matrix(prop.table(tab, 1), "row_proportion"),
          "results/tables/12-row-proportions.csv", row.names = FALSE)
write.csv(long_matrix(fit$stdres, "standardized_residual"),
          "results/tables/12-standardized-residuals.csv", row.names = FALSE)
write.csv(long_matrix(contribution, "chi_square_contribution"),
          "results/tables/12-cell-contributions.csv", row.names = FALSE)
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)
png("results/figures/12-preference-proportions.png", width = 1300, height = 800, res = 150)
par(mar = c(5, 5, 4, 2) + 0.1)
barplot(t(prop.table(tab, 1)), beside = TRUE,
        col = c("#087f8c", "#b86e13", "#594b9a"), ylim = c(0, 0.65),
        ylab = "Proportion within experience group", xlab = "Experience",
        main = "Synthetic learning preferences",
        legend.text = colnames(tab), args.legend = list(x = "topright", bty = "n"))
invisible(dev.off())
