## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4
)

## ----setup--------------------------------------------------------------------
library(mediaPlanR)

## ----sainsbury----------------------------------------------------------------
result <- calc_sainsbury(
  audiences = c(300000, 180000, 120000),
  population = 1000000,
  insertions = c(4, 6, 10)
)
result

## ----invariants---------------------------------------------------------------
sum(result$distribution$percent)
result$reach$percent
sum(seq_along(result$distribution$people) * result$distribution$people) /
  result$reach$people

## ----duplication--------------------------------------------------------------
data(ratings_example)
data(duplication_example)

reach_sainsbury <- do.call(calc_sainsbury, ratings_example)
reach_binomial <- do.call(calc_binomial, ratings_example)
reach_agostini <- do.call(calc_agostini_duplication, duplication_example)
reach_hofmans <- do.call(calc_hofmans_duplication, duplication_example)
reach_metheringham <- calc_metheringham(
  audiences = duplication_example$audiences,
  insertions = c(1, 1, 1),
  duplication_matrix = duplication_example$duplication_matrix,
  population = duplication_example$population
)

data.frame(
  model = c("Sainsbury", "Binomial", "Agostini (k = 1.125)", "Hofmans (duplication)",
            "Metheringham"),
  reach_pct = round(c(
    reach_sainsbury$reach$percent,
    reach_binomial$reach$percent,
    reach_agostini$reach$percent,
    reach_hofmans$reach$percent,
    reach_metheringham$reach$percent
  ), 2)
)

## ----hbbd---------------------------------------------------------------------
# Step 1: reach of the plan from the audiences and the pairwise duplications
hofmans <- do.call(calc_hofmans_duplication, duplication_example)

# Step 2: Beta-Binomial with that reach and the mean implied by the audiences
hbbd <- fit_bbd_to_reach(
  insertions = rep(1, length(duplication_example$audiences)),
  audiences = duplication_example$audiences,
  reach = hofmans$reach$people,
  population = duplication_example$population
)

hbbd$parameters$converged
round(hbbd$distribution, 4)

## ----metheringham-------------------------------------------------------------
data(metheringham_example)
metheringham <- do.call(calc_metheringham, metheringham_example)
metheringham$reach$percent
length(metheringham$distribution$percent)   # zero to N = 12 exposures

# The mean of the distribution equals the plan's gross exposures per person
sum(seq_along(metheringham$distribution$percent) *
      metheringham$distribution$percent) / 100
with(metheringham_example, sum(audiences * insertions) / population)

## ----accumulation-------------------------------------------------------------
beta_binomial <- calc_beta_binomial(R1 = 0.50, R2 = 0.55, insertions = 5,
                                    population = 1000000)
beta_binomial$reach$percent

hofmans <- calc_hofmans_accumulation(R1 = 0.06, R2 = 0.103, insertions = 5)
hofmans$results

## ----nbd----------------------------------------------------------------------
counts <- c(rep(0, 40), rep(1, 25), rep(2, 15), rep(3, 8), 5, 7)
nbd_fit <- fit_nbd_exposure(counts)
nbd_fit$estimates

nbd_scenario <- nbd_exposure_distribution(
  mean_contacts = 2.5, size = 1.7, report_max = 9, opportunities = 9
)
nbd_scenario$diagnostics$probability_above_opportunities

## ----canex--------------------------------------------------------------------
data(canex_example)
canex_result <- do.call(calc_canex, canex_example)
canex_result
canex_result$diagnostics

## ----csd----------------------------------------------------------------------
data(csd_example)
csd_result <- do.call(calc_csd, csd_example)
csd_result$reach
csd_result$distribution
csd_result$steps

## ----msad---------------------------------------------------------------------
data(msad_example)
msad_result <- do.call(calc_msad, msad_example)
msad_result$reach
msad_result$steps
msad_result$diagnostics

## ----evaluation---------------------------------------------------------------
# Invented counts, for illustration only: they are not real data
observed_counts <- data.frame(
  contacts = 0:9,
  observed = c(520, 150, 130, 80, 50, 40, 20, 5, 3, 2)
)

csd_evaluation <- evaluate_exposure_model(
  observed = observed_counts,
  predicted = csd_result,
  observed_scale = "count"
)

csd_evaluation$summary
csd_evaluation$by_schedule

## ----mbd----------------------------------------------------------------------
data(mbd_example)
mbd_result <- do.call(calc_mbd, mbd_example)
mbd_result$reach
mbd_result$distribution

## ----mbd-diagnostics----------------------------------------------------------
mbd_result$diagnostics$negative_mass_adjusted
mbd_result$diagnostics$cells_adjusted

## ----mbd-mean-----------------------------------------------------------------
vehicles <- data.frame(insertions = c(2, 2), R1 = c(0.30, 0.20),
                       R2 = c(0.40, 0.30))
duplications <- matrix(c(0, 0.15, 0.15, 0), nrow = 2)
mbd_mean <- calc_mbd(vehicles, duplications, aggregation_order = "given")
unlist(mbd_mean$diagnostics[c("negative_mass_adjusted", "gross_mean_contacts",
                              "distribution_mean_contacts", "mean_error")])

## ----cbd----------------------------------------------------------------------
cbd_result <- do.call(calc_cbd, csd_example)
cbd_result$reach
cbd_result$distribution

## ----datasets-----------------------------------------------------------------
data(canex_example)
str(canex_example)
do.call(calc_canex, canex_example)$reach$percent

