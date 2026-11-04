# Run from repository root: Rscript scripts/R/23-reporting.R
# Base R. Generate a traceable numeric reporting example from synthetic data.
dat <- read.csv("data/example/23-reporting-scores.csv", stringsAsFactors = FALSE)
stopifnot(all(c("learner_id", "approach", "score") %in% names(dat)))
stopifnot(!anyNA(dat), !anyDuplicated(dat$learner_id))
stopifnot(all(nzchar(trimws(dat$learner_id))))
stopifnot(setequal(dat$approach, c("A", "B")), is.numeric(dat$score))
stopifnot(all(is.finite(dat$score)), all(dat$score >= 0 & dat$score <= 100))
a <- dat$score[dat$approach == "A"]; b <- dat$score[dat$approach == "B"]
stopifnot(length(a) >= 2, length(b) >= 2, sd(a) > 0, sd(b) > 0)
fit <- t.test(a, b, var.equal = FALSE, alternative = "two.sided", conf.level = 0.95)
result <- data.frame(n_A = length(a), n_B = length(b), mean_A = mean(a),
                     mean_B = mean(b), sd_A = sd(a), sd_B = sd(b),
                     difference_A_minus_B = mean(a) - mean(b),
                     lower_95 = fit$conf.int[1], upper_95 = fit$conf.int[2],
                     t = unname(fit$statistic), df = unname(fit$parameter), p_value = fit$p.value)
paragraph <- sprintf(paste0(
  "In the synthetic example, mean scores were %.2f points (SD %.2f, n=%d) in A ",
  "and %.2f points (SD %.2f, n=%d) in B. The estimated mean difference, A minus B, ",
  "was %.2f points (95%% Welch CI %.2f to %.2f). A two-sided Welch t-test gave ",
  "t(%.2f)=%.2f, p=%.4f. The interval includes zero and potentially important ",
  "differences; these results do not establish equivalence or a causal group effect."),
  mean(a), sd(a), length(a), mean(b), sd(b), length(b), mean(a)-mean(b),
  fit$conf.int[1], fit$conf.int[2], unname(fit$parameter), unname(fit$statistic), fit$p.value)
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("results/reports", recursive = TRUE, showWarnings = FALSE)
write.csv(result, "results/tables/23-reporting-evidence.csv", row.names = FALSE)
writeLines(c("# Synthetic statistical reporting example", "", paragraph, "",
             "## Human review required", "",
             "Verify study design, missingness, diagnostics, meaningful differences, and limitations.",
             "This generated paragraph is a teaching draft, not a complete applied report."),
           "results/reports/23-finding-draft.md")
capture.output(sessionInfo(), file = "results/reports/23-session-info.txt")
write.csv(data.frame(input = "data/example/23-reporting-scores.csv",
                     md5 = unname(tools::md5sum("data/example/23-reporting-scores.csv"))),
          "results/reports/23-input-checksum.csv", row.names = FALSE)
cat(paragraph, "\n")
