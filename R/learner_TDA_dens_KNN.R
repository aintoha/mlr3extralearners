#' @title Density Knn Density Learner
#' @author ain_toha
#' @name mlr_learners_dens.KNN
#'
#' @template class_learner
#' @templateVar id dens.KNN
#' @templateVar caller knnDE
#'
#'
#' @template seealso_learner
#' @template example
#' @export
LearnerDensKnn = R6Class("LearnerDensKnn",
  inherit = mlr3proba::LearnerDens,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(list(
        ParamDbl$new("k",
                     lower = 1, tags = "train",
                     special_vals = list("silver", "kung"))))
      ps$values = list(k = "silver")

      super$initialize(
        id = "dens.KNN",
        packages = "TDA",
        feature_types = c("integer", "numeric"),
        predict_types = c("pdf"),
        param_set = ps,
        man = "mlr3extralearners::mlr_learners_dens.KNN"
      )
    }


  ),

  private = list(
    .train = function(task) {

      data = task$data()[[1]]

      k = ifelse(self$param_set$values$k == "silver"| is.null(self$param_set$values$k), ceiling(length(data)^(0.5)),
                 ifelse(self$param_set$values$k == "kung", ceiling(length(data)), self$param_set$values$k))


      pdf <- function(x) {
      }
      body(pdf) <- substitute({
        mlr3misc::invoke(TDA::knnDE,
          X = matrix(data, ncol = 1),
          Grid = matrix(x, ncol = 1), k = k)
      })

      ps = distr6::ParameterSet$new(
        id = list("k"),
        value = list(k),
        support = list(
          set6::Reals$new()
        ))

      structure(list(distr = distr6::Distribution$new(
        name = paste("KNN Density"),
        short_name = paste0("knnDens"),
        pdf = pdf, type = set6::Reals$new(),
        parameters = ps)
      ))
    },

    .predict = function(task) {
      newdata = task$data()[[1]]
      list(pdf = self$model$distr$pdf(newdata), distr = self$model$distr)
    }
  )
)

.extralrns_dict$add("dens.KNN", LearnerDensKnn)
