# Run from repository root: Rscript scripts/R/21-multiple-testing.R
# Synthetic p-values illustrate adjustments; no underlying study is simulated.
dat <- read.csv("data/example/21-example-p-values.csv", stringsAsFactors = FALSE)
stopifnot(all(c("hypothesis", "p_value") %in% names(dat)))
stopifnot(!anyNA(dat), !anyDuplicated(dat$hypothesis))
stopifnot(all(nzchar(trimws(dat$hypothesis))), is.numeric(dat$p_value))
stopifnot(all(is.finite(dat$p_value)), all(dat$p_value >= 0 & dat$p_value <= 1))
alpha <- 0.05
dat$p_bonferroni <- p.adjust(dat$p_value, method = "bonferroni")
dat$p_holm <- p.adjust(dat$p_value, method = "holm")
dat$p_BH <- p.adjust(dat$p_value, method = "BH")
dat$reject_unadjusted <- dat$p_value <= alpha
dat$reject_holm <- dat$p_holm <= alpha
dat$reject_BH <- dat$p_BH <= alpha
print(dat)
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)
write.csv(dat, "results/tables/21-adjusted-p-values.csv", row.names = FALSE)
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)
png("results/figures/21-adjustments.png", width = 1300, height = 800, res = 150)
matplot(seq_len(nrow(dat)), cbind(dat$p_value, dat$p_bonferroni, dat$p_holm, dat$p_BH),
        type = "b", pch = 1:4, lty = 1:4, log = "y", xaxt = "n",
        col = c("gray40", "#b86e13", "#087f8c", "#594b9a"),
        xlab = "Hypothesis", ylab = "P-value (log scale)",
        main = "Six-hypothesis synthetic adjustment example")
axis(1, at = seq_len(nrow(dat)), labels = dat$hypothesis)
abline(h = alpha, lty = 2)
legend("topleft", legend = c("Raw", "Bonferroni", "Holm", "BH"),
       col = c("gray40", "#b86e13", "#087f8c", "#594b9a"), pch = 1:4, lty = 1:4, bty = "n")
invisible(dev.off())
