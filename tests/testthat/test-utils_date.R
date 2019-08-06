context("Ensuring that the string date functions work as expected")

test_that("the `to_short_year()` function works correctly", {
  
  to_short_year(date = "2015-06-15") %>%
    expect_equal("15")
  
  to_short_year(date = "2000-06-15") %>%
    expect_equal("00")
  
  to_short_year(date = "1955-06-15") %>%
    expect_equal("55")
})
