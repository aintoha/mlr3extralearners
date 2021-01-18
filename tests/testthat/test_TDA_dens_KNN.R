install_learners("dens.KNN")
load_tests("dens.KNN")

test_that("autotest", {
  learner = LearnerDensKnn$new()
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})
