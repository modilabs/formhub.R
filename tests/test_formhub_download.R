library(testthat)

#test_dir("~/Code/formhub.R/tests/")

test_that("downloading public data (with public form) works", {
  good_eats <- formhubDownload("good_eats", uname="mberg")
  expect_true("risk_factor" %in% names(good_eats))
  expect_true(nrow(good_eats) > 1)
  
  # public data with a password also works, no matter what the password
  good_eats <- formhubDownload("good_eats", uname="mberg", pass="boguspassword")
  expect_true("risk_factor" %in% names(good_eats))
  expect_true(nrow(good_eats) > 1)
  
})

test_that("downloading private data works", {
  data <- formhubDownload("Private_Data_For_Testing", uname="formhub_r", 
                        pass="t3st~p4ss")
  expect_true("submit_date" %in% names(data))
  expect_true(nrow(data) > 0)
  expect_true(is.instant(data$submit_date))
  expect_true(is.factor(data$functional))
  expect_true(is.character(data$wp_id))
})


test_that("downloading public data (with private form) falls back gracefully", {
  data <- formhubDownload("Public_Data_Private_Schema", uname="formhub_r")
  expect_true(nrow(data) > 0)
  expect_true(is.instant(data$submit_date))
  expect_true(is.factor(data$functional))
  expect_true(is.character(data$wp_id))
})

test_that("downloaded data has a lastDownloaded timestamp within last 10 minutes", {
  good_eats <- formhubDownload("good_eats", uname="mberg")
  expect_true(as.numeric(now() - good_eats@lastDownloaded) < 600) # request did not take more than 10 minutes
  data <- formhubDownload("Public_Data_Private_Schema", uname="formhub_r")
  expect_true(as.numeric(now() - data@lastDownloaded) < 600) # request did not take more than 10 minutes
})

test_that("downloaded data can be saved to cacheDirectory", {
  good_eats <- formhubDownload("good_eats", uname="mberg", 
                               cacheDirectory="~/Code/formhub.R/tests/fixtures/")
  expect_true(file.exists("~/Code/formhub.R/tests/fixtures/mberg-good_eats.formhubData.RDS"))
  file.remove("~/Code/formhub.R/tests/fixtures/mberg-good_eats.formhubData.RDS")
})
