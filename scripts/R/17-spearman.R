# Run from repository root: Rscript scripts/R/17-spearman.R
# Base R only. Tied practice counts; explicitly use asymptotic p-value.
dat <- read.csv("data/example/17-practice-scores.csv", stringsAsFactors = FALSE)
stopifnot(all(c("learner_id", "practice_sessions", "score") %in% names(dat)))
stopifnot(!anyNA(dat), !anyDuplicated(dat$learner_id))
stopifnot(all(nzchar(trimws(dat$learner_id))))
stopifnot(is.numeric(dat$practice_sessions), is.numeric(dat$score))
stopifnot(all(is.finite(dat$practice_sessions)), all(is.finite(dat$score)))
stopifnot(all(dat$practice_sessions >= 0), all(dat$score >= 0 & dat$score <= 100))
stopifnot(nrow(dat) > 3, sd(dat$practice_sessions) > 0, sd(dat$score) > 0)
dat$practice_rank <- rank(dat$practice_sessions, ties.method = "average")
dat$score_rank <- rank(dat$score, ties.method = "average")
fit <- cor.test(dat$practice_sessions, dat$score, method = "spearman",
                alternative = "two.sided", exact = FALSE, continuity = FALSE)
rho <- cor(dat$practice_rank, dat$score_rank)
stopifnot(isTRUE(all.equal(rho, unname(fit$estimate))))
result <- data.frame(n = nrow(dat), spearman_rho = rho,
                     S_reported_by_R = unname(fit$statistic),
                     p_asymptotic = fit$p.value)
print(fit); print(result)
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)
write.csv(result, "results/tables/17-spearman.csv", row.names = FALSE)
write.csv(dat, "results/tables/17-ranks.csv", row.names = FALSE)
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)
png("results/figures/17-spearman-scatter.png", width = 1400, height = 700, res = 150)
par(mfrow = c(1, 2))
plot(dat$practice_sessions, dat$score, pch = 19, col = "#087f8c",
     main = "Original measurements", xlab = "Practice sessions", ylab = "Score (points)")
plot(dat$practice_rank, dat$score_rank, pch = 19, col = "#b86e13",
     main = "Average ranks", xlab = "Practice rank", ylab = "Score rank")
invisible(dev.off())
