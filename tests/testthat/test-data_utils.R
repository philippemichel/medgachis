test_that("summarize_outcomes works correctly", {
  outcomes <- c(95, 92, 88, 96, 89, 94, 91, 93)
  groups <- c(rep("intervention", 4), rep("control", 4))
  
  result <- summarize_outcomes(outcomes, groups)
  
  # Check structure
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_true(all(c("group", "n", "mean", "sd", "median", "min", "max") %in% names(result)))
  
  # Check specific values for control group
  control_row <- result[result$group == "control", ]
  expect_equal(control_row$n, 4)
  expect_equal(control_row$mean, mean(c(89, 94, 91, 93)))
  
  # Test error handling
  expect_error(summarize_outcomes("abc", groups))
  expect_error(summarize_outcomes(outcomes, c("a", "b")))
})

test_that("validate_study_data works correctly", {
  # Test valid data
  valid_data <- data.frame(
    patient_id = 1:10,
    cost = runif(10, 1000, 2000),
    outcome = runif(10, 0.8, 1.0)
  )
  expect_true(validate_study_data(valid_data, c("patient_id", "cost", "outcome")))
  
  # Test non-data.frame input
  expect_warning(validate_study_data("not a data frame", c("col1")))
  expect_false(suppressWarnings(validate_study_data("not a data frame", c("col1"))))
  
  # Test empty data frame
  empty_df <- data.frame()
  expect_warning(validate_study_data(empty_df, c("col1")))
  expect_false(suppressWarnings(validate_study_data(empty_df, c("col1"))))
  
  # Test missing columns
  expect_warning(validate_study_data(valid_data, c("patient_id", "missing_col")))
  expect_false(suppressWarnings(validate_study_data(valid_data, c("patient_id", "missing_col"))))
  
  # Test empty columns
  data_with_empty <- valid_data
  data_with_empty$empty_col <- NA
  expect_warning(validate_study_data(data_with_empty, c("patient_id", "cost")))
  expect_false(suppressWarnings(validate_study_data(data_with_empty, c("patient_id", "cost"))))
})