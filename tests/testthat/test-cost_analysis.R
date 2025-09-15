test_that("calculate_icer works correctly", {
  # Test basic calculation
  result <- calculate_icer(1500, 1200, 0.95, 0.85)
  expect_equal(result, 3000)
  
  # Test when intervention is cheaper and more effective
  result2 <- calculate_icer(1200, 1500, 0.95, 0.85)
  expect_equal(result2, -3000)
  
  # Test error handling for non-numeric inputs
  expect_error(calculate_icer("abc", 1200, 0.95, 0.85))
  expect_error(calculate_icer(1500, "abc", 0.95, 0.85))
  expect_error(calculate_icer(1500, 1200, "abc", 0.85))
  expect_error(calculate_icer(1500, 1200, 0.95, "abc"))
  
  # Test warning for zero effectiveness difference
  expect_warning(calculate_icer(1500, 1200, 0.85, 0.85))
})

test_that("calculate_anesthesia_cost works correctly", {
  # Test basic calculation
  result <- calculate_anesthesia_cost(150, 2, 45, 25)
  expect_equal(result, 370)
  
  # Test with zero duration
  result2 <- calculate_anesthesia_cost(150, 0, 45, 25)
  expect_equal(result2, 70)
  
  # Test error handling for non-numeric inputs
  expect_error(calculate_anesthesia_cost("abc", 2, 45, 25))
  expect_error(calculate_anesthesia_cost(150, "abc", 45, 25))
  expect_error(calculate_anesthesia_cost(150, 2, "abc", 25))
  expect_error(calculate_anesthesia_cost(150, 2, 45, "abc"))
  
  # Test error for negative duration
  expect_error(calculate_anesthesia_cost(150, -1, 45, 25))
})