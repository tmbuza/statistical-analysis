# Chapter 02: Statistical inference from synthetic summary statistics.
# Run from the repository root:
# Rscript scripts/R/02-statistical-inference.R
# Uses base R only. Summary statistics cannot validate the study design.

# Summary statistics for the synthetic example.
n <- 25
sample_mean <- 72
sample_sd <- 10

# Estimate precision and calculate the t-based interval.
standard_error <- sample_sd / sqrt(n)
critical_value <- qt(0.975, df = n - 1)
margin <- critical_value * standard_error

c(
  mean = sample_mean,
  standard_error = standard_error,
  lower_95 = sample_mean - margin,
  upper_95 = sample_mean + margin
)

# Run after the preceding summary-statistic code.
reference_mean <- 70

t_statistic <- (sample_mean - reference_mean) / standard_error
p_value <- 2 * pt(-abs(t_statistic), df = n - 1)

c(t = t_statistic, df = n - 1, p_value = p_value)
