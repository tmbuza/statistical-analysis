# Run from repository root: Rscript scripts/R/15-mcnemar.R
# Base R only. Aggregated transition counts for complete paired binary outcomes.
dat <- read.csv("data/example/15-paired-binary-counts.csv", stringsAsFactors = FALSE)
stopifnot(all(c("before", "after", "count") %in% names(dat)))
stopifnot(!anyNA(dat), !anyDuplicated(dat[c("before", "after")]))
stopifnot(nrow(dat) == 4, setequal(dat$before, c("No", "Yes")))
stopifnot(setequal(dat$after, c("No", "Yes")))
stopifnot(is.numeric(dat$count), all(is.finite(dat$count)))
stopifnot(all(dat$count >= 0), all(dat$count == floor(dat$count)))
dat$before <- factor(dat$before, levels = c("No", "Yes"))
dat$after <- factor(dat$after, levels = c("No", "Yes"))
tab <- xtabs(count ~ before + after, data = dat)
n <- sum(tab)
gain <- unname(tab["No", "Yes"])
loss <- unname(tab["Yes", "No"])
discordant <- gain + loss
stopifnot(n > 0, discordant > 0)
# Primary test: exact conditional binomial test among discordant pairs.
exact <- binom.test(gain, discordant, p = 0.5, alternative = "two.sided")
uncorrected <- mcnemar.test(tab, correct = FALSE)
corrected <- mcnemar.test(tab, correct = TRUE)
result <- data.frame(
  complete_pairs = n, gains = gain, losses = loss, discordant_pairs = discordant,
  proportion_before = sum(tab["Yes", ]) / n,
  proportion_after = sum(tab[, "Yes"]) / n,
  difference_after_minus_before = (gain - loss) / n,
  exact_conditional_p = exact$p.value,
  X_squared_uncorrected = unname(uncorrected$statistic),
  approximate_p_uncorrected = uncorrected$p.value,
  X_squared_corrected = unname(corrected$statistic),
  approximate_p_corrected = corrected$p.value
)
print(tab); print(exact); print(uncorrected); print(corrected); print(result)
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)
write.csv(result, "results/tables/15-mcnemar.csv", row.names = FALSE)
# exact$conf.int concerns gains among discordants, NOT the marginal paired difference.
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)
png("results/figures/15-paired-transitions.png", width = 1300, height = 750, res = 150)
par(mar = c(7, 5, 4, 2) + 0.1)
counts <- c(tab["No", "No"], gain, loss, tab["Yes", "Yes"])
bars <- barplot(counts, names.arg = c("No to No", "No to Yes", "Yes to No", "Yes to Yes"),
                col = c("gray65", "#087f8c", "#b86e13", "gray65"),
                ylim = c(0, max(counts) * 1.2), ylab = "Number of participants",
                main = "Synthetic paired binary transitions")
text(bars, counts + 0.5, labels = counts)
invisible(dev.off())
