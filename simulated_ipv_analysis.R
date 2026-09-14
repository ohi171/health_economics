# SYNTHETIC DATA ONLY: not NFHS data or empirical evidence.
# Base R demonstration: healthcare contact x partner control.
# See README.md for motivation, interpretation, and limitations.

set.seed(20260914)
out_dir <- file.path(getwd(), "SIMULATED_IPV_analysis")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
n <- 10000
z <- qnorm(0.975)

# 1. Simulate an eligible sample.
dat <- data.frame(
  id = seq_len(n), synthetic = TRUE,
  age = sample(18:49, n, replace = TRUE),
  ever_married = 1L,
  education_years = pmin(18, pmax(0, round(rnorm(n, 8, 4)))),
  wealth = sample(1:5, n, replace = TRUE),
  urban = rbinom(n, 1, 0.35)
)
dat$age10 <- (dat$age - 33) / 10
dat$education5 <- (dat$education_years - 8) / 5
dat$wealth_c <- dat$wealth - 3
dat$partner_control <- rbinom(
  n, 1, plogis(0.20 - 0.15 * dat$education5 - 0.10 * dat$wealth_c)
)
dat$sexual_ipv <- rbinom(
  n, 1, plogis(-1.50 + 0.65 * dat$partner_control)
)
dat$physical_ipv <- ifelse(
  dat$sexual_ipv == 0, 1L, rbinom(n, 1, 0.80)
)
dat$severe_physical <- dat$physical_ipv * rbinom(
  n, 1, plogis(-1.10 + 0.60 * dat$partner_control)
)
dat$health_contact <- rbinom(
  n, 1, plogis(
    -0.70 - 0.45 * dat$partner_control +
      0.65 * dat$severe_physical + 0.20 * dat$urban +
      0.15 * dat$wealth_c + 0.15 * dat$education5
  )
)
# Deliberately imposed contact log-OR:
# without control = 0.90; with control = 0.90 - 1.20 = -0.30.
# These coefficients are invented assumptions.
eta <- with(dat,
  -4.80 + 0.90 * health_contact + 0.70 * partner_control -
    1.20 * health_contact * partner_control +
    0.85 * severe_physical + 0.45 * sexual_ipv +
    0.10 * age10 + 0.15 * education5 + 0.15 * wealth_c + 0.15 * urban
)
dat$formal_help <- rbinom(n, 1, plogis(eta))
stopifnot(
  all(dat$age >= 18 & dat$age <= 49),
  all(dat$ever_married == 1),
  all(dat$physical_ipv == 1 | dat$sexual_ipv == 1),
  all(complete.cases(dat))
)

save_csv <- function(x, filename) {
  write.csv(x, file.path(out_dir, filename), row.names = FALSE)
}
save_csv(dat, "SIMULATED_ipv_data.csv")

# 2. Variable definitions (fictional concepts, not NFHS recodes).
dictionary <- data.frame(
  variable = c(
    "id", "synthetic", "age", "ever_married", "education_years",
    "wealth", "urban", "partner_control", "physical_ipv", "sexual_ipv",
    "severe_physical", "health_contact", "formal_help",
    "age10", "education5", "wealth_c"
  ),
  definition = c(
    "Fictional participant identifier",
    "TRUE: every observation is synthetic",
    "Age in completed years: 18-49",
    "Ever married: all observations equal 1",
    "Completed education years: 0-18",
    "Ordered wealth category: 1 lowest to 5 highest",
    "Urban residence: 1 yes, 0 no",
    "Any reported partner controlling behaviour: 1 yes, 0 no",
    "Past-year physical partner violence: 1 yes, 0 no",
    "Past-year sexual partner violence: 1 yes, 0 no",
    "Past-year severe physical partner violence: 1 yes, 0 no",
    "Healthcare contact in past three months: 1 yes, 0 no",
    "Formal violence-related help in past year: 1 yes, 0 no",
    "(Age - 33) / 10", "(Education years - 8) / 5", "Wealth - 3"
  )
)
# Formal sources in this example: health professionals, police,
# legal services, and support organisations.
save_csv(dictionary, "SIMULATED_variable_dictionary.csv")

# 3. Unadjusted group summaries.
# Order: contact/control = 0/0, 1/0, 0/1, 1/1.
groups <- expand.grid(health_contact = 0:1, partner_control = 0:1)
descriptive <- do.call(rbind, lapply(seq_len(nrow(groups)), function(i) {
  keep <- with(dat,
    health_contact == groups$health_contact[i] &
      partner_control == groups$partner_control[i]
  )
  data.frame(
    groups[i, ], n = sum(keep),
    formal_help_events = sum(dat$formal_help[keep]),
    unadjusted_help_percent = 100 * mean(dat$formal_help[keep])
  )
}))
cat("\nSIMULATED descriptive results:\n")
print(descriptive, row.names = FALSE)
save_csv(descriptive, "SIMULATED_descriptive_results.csv")

# 4. Adjusted logistic regression.
# Model-based inference for an independent synthetic sample.
fit <- glm(
  formal_help ~ health_contact * partner_control +
    severe_physical + sexual_ipv + age10 + education5 + wealth_c + urban,
  family = binomial(), data = dat
)
stopifnot(fit$converged, all(is.finite(coef(fit))))
b <- coef(fit)
V <- vcov(fit)
se_b <- sqrt(diag(V))
or_table <- data.frame(
  term = names(b),
  log_odds_coefficient = unname(b),
  adjusted_OR = exp(unname(b)),
  CI_low = exp(unname(b - z * se_b)),
  CI_high = exp(unname(b + z * se_b)),
  p_value = 2 * pnorm(-abs(unname(b / se_b)))
)
save_csv(or_table, "SIMULATED_logistic_regression.csv")

# 5. Standardise all four combinations to the SAME covariate distribution.
# Delta-method inference treats the covariate distribution as fixed.
standardise <- function(contact, control) {
  nd <- dat
  nd$health_contact <- contact
  nd$partner_control <- control
  X <- model.matrix(
    delete.response(terms(fit)), data = nd,
    contrasts.arg = fit$contrasts, xlev = fit$xlevels
  )
  X <- X[, names(b), drop = FALSE]
  p <- plogis(drop(X %*% b))
  list(
    probability = mean(p),
    gradient = colMeans(X * as.numeric(p * (1 - p)))
  )
}
std <- lapply(seq_len(nrow(groups)), function(i) {
  standardise(groups$health_contact[i], groups$partner_control[i])
})
prob <- vapply(std, function(x) x$probability, numeric(1))
G <- do.call(rbind, lapply(std, function(x) x$gradient))
S <- G %*% V %*% t(G)
se_prob <- sqrt(diag(S))
se_logit_prob <- se_prob / (prob * (1 - prob))
adjusted <- data.frame(
  groups,
  probability_percent = 100 * prob,
  CI_low_percent = 100 * plogis(qlogis(prob) - z * se_logit_prob),
  CI_high_percent = 100 * plogis(qlogis(prob) + z * se_logit_prob)
)
cat("\nSIMULATED adjusted probabilities (%):\n")
print(adjusted, row.names = FALSE)
save_csv(adjusted, "SIMULATED_adjusted_probabilities.csv")

# 6. Central result: interaction on the probability scale.
# Third row = contact difference with control minus without control.
# This contrast is not a causal difference-in-differences design.
A <- rbind(
  contact_difference_no_control = c(-1, 1, 0, 0),
  contact_difference_control = c(0, 0, -1, 1),
  difference_between_contact_differences = c(1, -1, -1, 1)
)
contrast_est <- drop(A %*% prob)
contrast_se <- sqrt(diag(A %*% S %*% t(A)))
contrasts <- data.frame(
  contrast = rownames(A),
  estimate_percentage_points = 100 * contrast_est,
  CI_low_percentage_points = 100 * (contrast_est - z * contrast_se),
  CI_high_percentage_points = 100 * (contrast_est + z * contrast_se),
  p_value = 2 * pnorm(-abs(contrast_est / contrast_se))
)
cat("\nSIMULATED central contrasts (percentage points):\n")
print(contrasts, row.names = FALSE)
save_csv(contrasts, "SIMULATED_central_interaction_results.csv")

# 7. Plot standardised probabilities with 95% confidence intervals.
draw_figure <- function() {
  cols <- c("#0072B2", "#D55E00")
  ymax <- max(adjusted$CI_high_percent) * 1.20
  plot(
    NA, xlim = c(-0.12, 1.12), ylim = c(0, ymax), xaxt = "n",
    xlab = "Healthcare contact in past three months",
    ylab = "Adjusted probability of formal help-seeking (%)",
    main = "SIMULATED ONLY: healthcare contact and partner control"
  )
  axis(1, at = c(0, 1), labels = c("No contact", "Contact"))
  for (control in 0:1) {
    idx <- which(adjusted$partner_control == control)
    x <- adjusted$health_contact[idx] +
      ifelse(control == 0, -0.025, 0.025)
    lines(
      x, adjusted$probability_percent[idx], type = "b",
      pch = 16 + control, lwd = 2, col = cols[control + 1]
    )
    arrows(
      x0 = x, y0 = adjusted$CI_low_percent[idx],
      x1 = x, y1 = adjusted$CI_high_percent[idx],
      angle = 90, code = 3, length = 0.06, col = cols[control + 1]
    )
  }
  legend(
    "topleft",
    legend = c("No reported partner control", "Reported partner control"),
    col = cols, pch = c(16, 17), lwd = 2, bty = "n", cex = 0.85
  )
}
png(
  file.path(out_dir, "SIMULATED_interaction_figure.png"),
  width = 1600, height = 1100, res = 180
)
draw_figure()
dev.off()
if (interactive()) draw_figure()

# 8. Generate a clearly labelled results paragraph.
describe_contrast <- function(i) {
  sprintf(
    "%.2f percentage points (95%% CI %.2f to %.2f)",
    contrasts$estimate_percentage_points[i],
    contrasts$CI_low_percentage_points[i],
    contrasts$CI_high_percentage_points[i]
  )
}
report <- paste0(
  "SIMULATED RESULTS ONLY - NOT EMPIRICAL EVIDENCE\n\n",
  "The fictional sample included ", nrow(dat),
  " ever-married women aged 18-49 reporting past-year physical ",
  "or sexual partner violence. There were ",
  sum(dat$formal_help), " simulated formal help-seeking events.\n\n",
  "After adjustment for severe physical violence, sexual violence, ",
  "age, education, household wealth and urban residence, the ",
  "standardised probability difference comparing healthcare contact ",
  "with no contact was ", describe_contrast(1),
  " among women without reported partner control and ",
  describe_contrast(2), " among women reporting partner control. ",
  "The latter minus the former difference was ",
  describe_contrast(3), " (p = ",
  format.pval(contrasts$p_value[3], digits = 3, eps = 0.001),
  ").\n\n",
  "These results arise from invented data with an interaction ",
  "deliberately specified in the simulation. They demonstrate ",
  "an analysis workflow and do not establish a real association ",
  "or a causal effect."
)
writeLines(report, file.path(out_dir, "SIMULATED_results_paragraph.txt"))
writeLines(
  capture.output(summary(fit)),
  file.path(out_dir, "SIMULATED_model_summary.txt")
)
writeLines(
  capture.output(sessionInfo()),
  file.path(out_dir, "SIMULATED_session_info.txt")
)
cat("\n", report, "\n\n", sep = "")
cat("Output files saved in:\n", normalizePath(out_dir), "\n")
