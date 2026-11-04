# Run from repository root: Rscript scripts/R/13-fisher-exact.R
# Base R only. Independent synthetic participants, aggregated counts.
dat <- read.csv("data/example/13-completion-counts.csv", stringsAsFactors = FALSE)
stopifnot(all(c("group", "outcome", "count") %in% names(dat)))
stopifnot(!anyNA(dat), !anyDuplicated(dat[c("group", "outcome")]))
stopifnot(nrow(dat) == 4, setequal(dat$group, c("A", "B")))
stopifnot(setequal(dat$outcome, c("Completed", "Not_completed")))
stopifnot(is.numeric(dat$count), all(is.finite(dat$count)))
stopifnot(all(dat$count >= 0), all(dat$count == floor(dat$count)))
dat$group <- factor(dat$group, levels = c("A", "B"))
dat$outcome <- factor(dat$outcome, levels = c("Completed", "Not_completed"))
tab <- xtabs(count ~ group + outcome, data = dat)
stopifnot(all(rowSums(tab) > 0), all(colSums(tab) > 0))
fit <- fisher.test(tab, alternative = "two.sided", conf.level = 0.95)
# Cross-product OR is different from fisher.test's conditional MLE.
sample_or <- (tab[1, 1] * tab[2, 2]) / (tab[1, 2] * tab[2, 1])
result <- data.frame(N = sum(tab), proportion_A = tab[1, 1] / sum(tab[1, ]),
                     proportion_B = tab[2, 1] / sum(tab[2, ]),
                     cross_product_OR = sample_or,
                     conditional_OR = unname(fit$estimate),
                     exact_ci_lower = fit$conf.int[1], exact_ci_upper = fit$conf.int[2],
                     exact_two_sided_p = fit$p.value)
print(tab); print(fit); print(result)
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)
write.csv(result, "results/tables/13-fisher-exact.csv", row.names = FALSE)
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)
png("results/figures/13-completion-proportions.png", width = 1100, height = 750, res = 150)
p <- tab[, 1] / rowSums(tab)
bars <- barplot(p, ylim = c(0, 1), col = c("#087f8c", "#b86e13"),
                ylab = "Observed completion proportion", xlab = "Group",
                main = "Synthetic completion outcomes")
text(bars, p + 0.05, labels = paste0(tab[, 1], "/", rowSums(tab)))
invisible(dev.off())
