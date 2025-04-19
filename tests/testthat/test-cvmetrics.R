test_that("cvmetrics returns a dataframe with the correct number of columns and data types", {
  # Sample test dataset
  # just a reminder: usethis::use_test("cvmetrics") to create a test file
  ids <- seq(1,5)
  combs <- expand.grid(ID1 = ids, ID2 = ids) %>% filter(ID1 != ID2)
  df <- data.frame(
    dist = sample(1:50, 40, replace = T),
    yday = rep(1:2, each = 20),
    year = rep(2000:2001, each = 20),
    id1 = rep(combs$ID1, 2),
    id2 = rep(combs$ID2, 2)
  )

  # Assuming iidist() calculates inter-individual distances for a given time
  result <- cvmetrics(df, iidist_col = "dist", id_col = "id1", grp_by = c("yday", "year"), log10 = F)
  grp_by = c("yday", "year")
  expect_s3_class(result, "tbl_df")
  expect_length(result, length(grp_by) + 3)
  expect_type(result$cvind, "double")
  expect_true(all(!is.na(result)))
})
