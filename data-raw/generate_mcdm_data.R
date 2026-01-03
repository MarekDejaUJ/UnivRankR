# This script generates the internal dataset for the package

set.seed(123)

mcdm_raw_data <- data.frame(
  # --- Criterion 1: Cost Items (Continuous) ---
  cost_raw_mat = runif(20, 500, 2000),
  cost_labor   = runif(20, 200, 800),

  # --- Criterion 2: Quality Items (Likert 1-5) ---
  qual_durability = sample(1:5, 20, replace=TRUE),
  qual_finish     = sample(c(1:5, 99), 20, replace=TRUE, prob=c(rep(0.18,5), 0.1)),
  qual_defects    = sample(1:5, 20, replace=TRUE),
  qual_ux         = sample(1:5, 20, replace=TRUE),

  # --- Criterion 3: Delivery Items (Days) ---
  del_time_avg = runif(20, 2, 14),
  del_reliability = runif(20, 80, 100),
  del_tracking    = sample(0:1, 20, replace=TRUE) * 10,

  # --- Criterion 4: Sustainability (Likert 1-7) ---
  sus_co2      = sample(1:7, 20, replace=TRUE),
  sus_waste    = sample(1:7, 20, replace=TRUE),
  sus_material = sample(c(1:7, NA), 20, replace=TRUE),
  sus_social   = sample(1:7, 20, replace=TRUE)
)

# Save the data into the package's data/ directory
usethis::use_data(mcdm_raw_data, overwrite = TRUE)
