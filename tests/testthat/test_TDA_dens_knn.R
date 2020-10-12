install_learners("dens.knn")

test_that("autotest", {
  learner = LearnerDensKnn$new()
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})
