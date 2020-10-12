library(mlr3extralearners)
install_learners("dens.knn")

test_that("dens.knn train", {
  learner = lrn("dens.knn")
  fun = TDA::knnDE
  exclude = c(
    "X", #handled in predict
    "Grid" #handled in predict
  )

  ParamTest = run_paramtest(learner, fun, exclude)
  expect_true(ParamTest, info = paste0(
    "Missing parameters:",
    paste0("- '", ParamTest$missing, "'", collapse = "\n")))
})


