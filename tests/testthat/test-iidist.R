test_that("iidist returns a dataframe with the correct number of columns and data types", {
  # just a reminder: usethis::use_test("cvmetrics") to create a test file
  # Sample test dataset
  df <- data.frame(
    x = sample(-180:180, 25),
    y = sample(-90:90, 25),
    yday = rep(1:5, each = 5),
    id = rep(1:5, times = 5)
  )
  df <- sf::st_as_sf(df, coords = c("x","y"), crs = 4329)

  # Assuming iidist() calculates inter-individual distances for a given time
  result <- iidist(df, nest_by = c("yday"), idcol = "id")
  nest_by = c("yday")
  expect_s3_class(result, "tbl_df")
  expect_length(result, length(nest_by) + 3)
  expect_s3_class(result$iidist, "units")
  expect_true(all(!is.na(result)))
})
