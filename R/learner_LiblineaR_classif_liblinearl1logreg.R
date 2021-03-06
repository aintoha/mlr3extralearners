#' @title L1-Regularized Logistic Regression Learner
#' @author be-marc
#' @name mlr_learners_classif.liblinearl1logreg
#'
#' @template class_learner
#' @templateVar id classif.liblinearl1logreg
#' @templateVar caller LiblineaR
#'
#' @details Calls [LiblineaR::LiblineaR()] with `type = 6`.
#'
#' @export
#' @template seealso_learner
#' @template example
LearnerClassifLiblineaRL1LogReg = R6Class("LearnerClassifLiblineaRL1LogReg", # nolint
  inherit = LearnerClassif,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(
        params = list(
          ParamDbl$new(id = "cost", default = 1, lower = 0, tags = "train"),
          ParamDbl$new(id = "epsilon", default = 0.01, lower = 0, tags = "train"),
          ParamDbl$new(id = "bias", default = 1, tags = "train"),
          ParamInt$new(id = "cross", default = 0L, lower = 0L, tags = "train"),
          ParamLgl$new(id = "verbose", default = FALSE, tags = "train"),
          ParamUty$new(id = "wi", default = NULL, tags = "train"),
          ParamLgl$new(id = "findC", default = FALSE, tags = "train"),
          ParamLgl$new(id = "useInitC", default = TRUE, tags = "train")
        )
      )
      # 50 is an arbitrary choice here
      ps$add_dep("findC", "cross", CondAnyOf$new(seq(2:50)))
      ps$add_dep("useInitC", "findC", CondEqual$new(TRUE))

      super$initialize(
        id = "classif.liblinearl1logreg",
        packages = "LiblineaR",
        feature_types = "numeric",
        predict_types = c("response", "prob"),
        param_set = ps,
        properties = c("twoclass", "multiclass"),
        man = "mlr3extralearners::mlr_learners_classif.liblinearl1logreg"
      )
    }
  ),

  private = list(
    .train = function(task) {
      pars = self$param_set$get_values(tags = "train")
      data = task$data()
      train = data[, task$feature_names, with = FALSE]
      target = data[, task$target_names, with = FALSE]

      mlr3misc::invoke(LiblineaR::LiblineaR, data = train, target = target, type = 6L, .args = pars)
    },

    .predict = function(task) {
      newdata = task$data(cols = task$feature_names)

      if (self$predict_type == "response") {
        p = mlr3misc::invoke(predict, self$model, newx = newdata)
        list(response = p$predictions)
      } else {
        p = mlr3misc::invoke(predict, self$model, newx = newdata, proba = TRUE)
        list(prob = p$probabilities)
      }
    }
  )
)

lrns_dict$add("classif.liblinearl1logreg", LearnerClassifLiblineaRL1LogReg)
