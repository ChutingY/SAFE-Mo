simple_random <- data.frame(
  sigma = c(0.01045965, 0.01045965, 0.01045965, 0.01899138, 0.01899138, 0.01899138, 0.02752311, 0.02752311, 0.02752311),
  C = rep(c(0.25, 0.5, 1), 3),
  ROC = c(0.7950563, 0.7950605, 0.7902334, 0.7949614, 0.7876773, 0.7793229, 0.7910581, 0.7839119, 0.7729003),
  Sens = c(0.6825118, 0.6741967, 0.6559577, 0.6825529, 0.6536478, 0.6255236, 0.6698009, 0.6389714, 0.6043021),
  Spec = c(0.7480435, 0.7561977, 0.7718466, 0.7523957, 0.7691889, 0.7828314, 0.7597255, 0.7769454, 0.7914557)
)

stratified <- data.frame(
  sigma = c(0.01060198, 0.01060198, 0.01060198, 0.01901038, 0.01901038, 0.01901038, 0.02741879, 0.02741879, 0.02741879),
  C = rep(c(0.25, 0.5, 1), 3),
  ROC = c(0.8036010, 0.8014003, 0.7955913, 0.8012432, 0.7957696, 0.7838861, 0.7977896, 0.7887629, 0.7762213),
  Sens = c(0.7104606, 0.6976185, 0.6810493, 0.6961989, 0.6798860, 0.6441772, 0.6835224, 0.6516904, 0.6203631),
  Spec = c(0.7394907, 0.7489338, 0.7583005, 0.7440363, 0.7593552, 0.7722499, 0.7537764, 0.7705267, 0.7891030)
)

delta_calc <- function(metric) {
  data.frame(
    Avg_Simple = mean(simple_random[[metric]]),
    Avg_Stratified = mean(stratified[[metric]]),
    Delta = mean(stratified[[metric]]) - mean(simple_random[[metric]]),
    Rel_Improvement = (mean(stratified[[metric]]) - mean(simple_random[[metric]])) / mean(simple_random[[metric]]) * 100
  )
}

metrics <- c("ROC", "Sens", "Spec")
delta_results <- lapply(metrics, delta_calc)
names(delta_results) <- metrics
print(delta_results)
