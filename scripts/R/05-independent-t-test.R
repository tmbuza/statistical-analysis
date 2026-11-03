# Run from the repository root. Base R only.
x <- read.csv("data/example/independent-groups.csv")
stopifnot(!anyDuplicated(x$sample_id), all(x$group %in% c("A", "B")),
          all(is.finite(x$score)))
a <- x$score[x$group == "A"]
b <- x$score[x$group == "B"]
stopifnot(length(a) >= 2, length(b) >= 2)
# Ordering fixes the direction of the reported difference: B minus A.
fit <- t.test(b, a, var.equal = FALSE, conf.level = 0.95)
out <- data.frame(n_A = length(a), n_B = length(b),
                  mean_A = mean(a), mean_B = mean(b),
                  difference_B_minus_A = mean(b) - mean(a),
                  ci_low = fit$conf.int[1], ci_high = fit$conf.int[2],
                  t = unname(fit$statistic), df = unname(fit$parameter),
                  p_value = fit$p.value)
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)
write.table(out, "results/tables/05-welch-r.tsv", sep = "\t",
            row.names = FALSE, quote = FALSE)
print(out)
