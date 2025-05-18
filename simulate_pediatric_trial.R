# Simulate success of a 50 child trial with UPA, Risa
# Assumes pediatric remission 25% to 0% less than
# remission in adults
# UPA 208/716, Risa 132/650
# UPA

n <- 70 # number of children in single arm trial

results <- c(
  "Drug", "Percent threshold for 2.5th percentile",
  "Percent worse in children",
  "Success rate"
)
prior <- c(2, 5) # Deflated prior from adult data
for (ratio in c(0.25, 0.2, 0.15, 0.1, 0.05, 0)) # how much worse in children vs adults
{
  data <- rbinom(1000000, n, 208 / 716 * (1 - ratio))
  successes <- data + prior[1]
  failures <- n - data + prior[2]
  # 2.5th percentile from posterior distribution
  lower <- qbeta(0.025, successes, failures)
  results <- rbind(results, c("UPA", 0.10, ratio, mean(lower > 0.10)))
  results <- rbind(results, c("UPA", 0.15, ratio, mean(lower > 0.15)))
}
# Risa
prior <- c(1, 4) # Deflated prior from adult data
for (ratio in c(0.25, 0.2, 0.15, 0.10, 0.05, 0))
{
  data <- rbinom(1000000, n, 132 / 650 * (1 - ratio))
  successes <- data + prior[1]
  failures <- n - data + prior[2]
  # 2.5th percentile from posterior distribution
  lower <- qbeta(0.025, successes, failures)
  results <- rbind(results, c("Risa", 0.10, ratio, mean(lower > 0.10)))
  results <- rbind(results, c("Risa", 0.15, ratio, mean(lower > 0.15)))
}
write.table(results, "pediatric_results.txt", row.names = FALSE, col.names = FALSE, sep = ",")
