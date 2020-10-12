#' @title Density Knn Density Learner
#' @author aintoha
#' @name mlr_learners_dens.knn
#'
#' @template class_learner
#' @templateVar id dens.knn
#' @templateVar caller knnDE
#'
#'
#' @template seealso_learner
#' @template example
#' @export
LearnerDensKnn = R6Class("LearnerDensKnn",
  inherit = LearnerDens,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(
        params = list(
          ParamDbl$new(id = "k", lower = 1, tags = "train")
        )
      )

      super$initialize(
        id = "dens.knn",
        packages = "TDA",
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        predict_types = c("pdf", "cdf"),
        param_set = ps,
        man = "mlr3extralearners::mlr_learners_dens.knn"
      )
    }
  ),

  private = list(
    .train = function(task) {

      pars = self$param_set$get_values(tag = "train")

      data = task$truth()

      dat = as.matrix(data)

      pdf <- function(x) {
      }
      body(pdf) <- substitute({
        mlr3misc::invoke(TDA::knnDE, X = dat, Grid = as.matrix(x), .args = pars)
      })

      distr6::Distribution$new(
        name = "knn Density",
        short_name = "knnDens",
        pdf = pdf,
        type = set6::Reals$new())
    },

    .predict = function(task) {
      mlr3proba::PredictionDens$new(task = task, pdf = self$model$pdf(task$truth()))

    }
  )
)

lrns_dict$add("dens.knn", LearnerDensKnn)
