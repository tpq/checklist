test_that("rfe works with checklist loss", {

  set.seed(1)
  M <- matrix(sample(0:1, 1000, replace = T), 100, 10)
  colnames(M) <- paste0("V", 1:10)
  y <- M[,2] + M[,7] + rowSums(M[,2:8]) + 2*runif(100)
  res <- rfe(M, y, make_checklist_loss(5))

  expect_equal(
    res$Column[1],
    "V7"
  )

  expect_equal(
    res$Column[2],
    "V2"
  )
})
