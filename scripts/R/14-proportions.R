# Run from repository root: Rscript scripts/R/14-proportions.R
# Base R only. Two independent synthetic groups, binary completion outcome.
dat <- read.csv("data/example/14-completion-summary.csv", stringsAsFactors = FALSE)
stopifnot(all(c("group", "completed", "total") %in% names(dat)))
stopifnot(!anyNA(dat), !anyDuplicated(dat$group), setequal(dat$group, c("A", "B")))
stopifnot(is.numeric(dat$completed), is.numeric(dat$total))
stopifnot(all(is.finite(dat$completed)), all(is.finite(dat$total)))
stopifnot(all(dat$total > 0), all(dat$total == floor(dat$total)))
stopifnot(all(dat$completed >= 0), all(dat$completed <= dat$total))
stopifnot(all(dat$completed == floor(dat$completed)))
dat <- dat[match(c("A", "B"), dat$group), ]
p <- dat$completed / dat$total
# One-sample exact binomial example uses A against a fixed reference of 0.50.
one <- binom.test(dat$completed[1], dat$total[1], p = 0.50,
                  alternative = "two.sided", conf.level = 0.95)
# Two-sample approximate test, explicitly without continuity correction.
two <- prop.test(dat$completed, dat$total, alternative = "two.sided",
                 correct = FALSE, conf.level = 0.95)
print(one); print(two)
one_result <- data.frame(group = "A", completed = dat$completed[1], total = dat$total[1],
                         proportion = p[1], reference = 0.5,
                         ci_lower_exact = one$conf.int[1], ci_upper_exact = one$conf.int[2],
                         exact_p = one$p.value)
two_result <- data.frame(proportion_A = p[1], proportion_B = p[2],
                         difference_A_minus_B = p[1] - p[2],
                         ci_lower_unpooled_normal = two$conf.int[1],
                         ci_upper_unpooled_normal = two$conf.int[2],
                         X_squared = unname(two$statistic),
                         df = unname(two$parameter), approximate_p = two$p.value)
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)
write.csv(one_result, "results/tables/14-one-proportion.csv", row.names = FALSE)
write.csv(two_result, "results/tables/14-two-proportions.csv", row.names = FALSE)
# Separate exact intervals for each proportion: descriptive, not a difference interval.
intervals <- t(vapply(seq_len(nrow(dat)), function(i) {
  unname(binom.test(dat$completed[i], dat$total[i])$conf.int)
}, numeric(2)))
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)
png("results/figures/14-completion-proportions.png", width = 1100, height = 750, res = 150)
plot(1:2, p, ylim = c(0, 1), xaxt = "n", xlim = c(0.5, 2.5), pch = 19,
     col = "#087f8c", xlab = "Group", ylab = "Completion proportion",
     main = "Proportions with individual 95% exact binomial intervals")
axis(1, at = 1:2, labels = dat$group)
arrows(1:2, intervals[, 1], 1:2, intervals[, 2], angle = 90, code = 3,
       length = 0.08, col = "#087f8c")
abline(h = 0.5, lty = 2, col = "gray50")
invisible(dev.off())
