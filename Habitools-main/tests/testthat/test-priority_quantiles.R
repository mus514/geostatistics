test_that("error when `direction` is null and only one `probs` is determined", {
  r <- terra::rast(system.file("ex/elev.tif", package = "terra"))
  expect_error(topPrio(r = r, probs = 0.1))
})
