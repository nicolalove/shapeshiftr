test_that("cvratio returns a dataframe OR numeric value with the correct values", {
  # just a reminder: usethis::use_test("cvmetrics") to create a test file
  # just a reminder: testthat::test_file("tests/testthat/test-cvpop.R") to test a test file
  # just a dataset with points. I put random stuff in there to make sure it didn't interfere
  df <- data.frame(
    x = sample(-100:100, 50),
    y = sample(-100:100, 50),
    yday = rep(1:10, each = 5),
    id = rep(1:10, times = 5),
    random = rep("char", 50)
  )
  lon <- sample(-180:180, 25)
  lat <- sample(-90:90, 25)

  result1 <- cvratio(df[,c("x", "y")], log10 = T) # if not accounting for yday
  result2 <- df %>% group_by(random) %>% summarise(ratio = cvratio(across(c(x,y)), log10 = T),
                                                   ind = cvind(across(c(x,y)), log10 = T),
                                                   pop = cvpop(across(c(x,y)), log10 = T),
                                                   test = ind - pop)
  expect_type(result1, "double")
  expect_true(!is.na(result1))
  expect_equal(result1, result2$ratio)
  expect_equal(result2$ratio, result2$test)

  result3 <- cvratio(cbind(lon, lat))
  expect_type(result3, "double")
  expect_true(!is.na(result3))

  m <- as.matrix(cbind(lon, lat))
  result4 <- cvratio(m)
  expect_type(result4, "double")
  expect_true(!is.na(result4))
  expect_equal(result3, result4)

  expect_error(cvratio(c(lon, lat)))

})
