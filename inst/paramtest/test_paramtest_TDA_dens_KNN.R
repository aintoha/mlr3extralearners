library(mlr3extralearners)
install_learners("dens.KNN")

test_that("dens.KNN train", {
  learner = lrn("dens.KNN")
  fun = TDA::knnDE
  exclude = c(
  )

  ParamTest = run_paramtest(learner, fun, exclude)
  expect_true(ParamTest, info = paste0(
    "Missing parameters:",
    paste0("- '", ParamTest$missing, "'", collapse = "
")))
})

test_that("dens.KNN predict", {
  learner = lrn("dens.KNN")
  fun = TDA:::predict
    exclude = c(
    )

  ParamTest = run_paramtest(learner, fun, exclude)
  expect_true(ParamTest, info = paste0(
    "Missing parameters:",
    paste0("- '", ParamTest$missing, "'", collapse = "
")))
})
