#' @title Density Nonparametric Learner
#' @name mlr_learners_dens.nonpar
#' @author RaphaelS1
#'
#' @template class_learner
#' @templateVar id dens.nonpar
#' @templateVar caller sm.density
#'
#' @references
#' Bowman, A.W. and Azzalini, A. (1997).
#' Applied Smoothing Techniques for Data Analysis: the Kernel Approach with S-Plus Illustrations.
#' Oxford University Press, Oxford.
#'
#' @template seealso_learner
#' @template example
#' @export
LearnerDensNonparametric = R6Class("LearnerDensNonparametric",
  inherit = mlr3proba::LearnerDens,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(
        params = list(
          ParamDbl$new(id = "h", tags = "train"),
          ParamUty$new(id = "group", tags = "train"),
          ParamDbl$new(id = "delta", tags = "train"),
          ParamDbl$new(id = "h.weights", default = 1, tags = "train"),
          ParamUty$new(id = "hmult", default = 1, tags = "train"),
          ParamFct$new(
            id = "method", default = "normal",
            levels = c("normal", "cv", "sj", "df", "aicc"), tags = "train"),
          ParamLgl$new(id = "positive", default = FALSE, tags = "train"),
          ParamUty$new(id = "verbose", default = 1, tags = "train")
        )
      )

      super$initialize(
        id = "dens.nonpar",
        packages = "sm",
        feature_types = c("integer", "numeric"),
        predict_types = "pdf",
        param_set = ps,
        properties = "weights",
        man = "mlr3extralearners::mlr_learners_dens.nonpar"
      )
    }
  ),

  private = list(
    .train = function(task) {

      pars = self$param_set$get_values(tag = "train")

      if ("weights" %in% task$properties) {
        pars$weights = task$weights$weight
      }

      if (is.null(pars$h)) {
        bw = sm::hnorm(x = task$data()[[1]])
      } else {
        bw = pars$h
      }

      pdf = function(x) {
      } # nolint
      body(pdf) = substitute({
        mlr3misc::invoke(sm::sm.density,
          x = data, eval.points = x, display = "none", show.script = FALSE,
          .args = pars)$estimate
      }, list(data = task$data()[[1]]))


      ps = ParameterSet$new(
        id = list("bandwidth", "kernel"),
        value = list(bw, "Norm"),
        support = list(
          set6::Reals$new(),
          set6::Set$new(elements = as.list(distr6::listKernels()[, 1]))
      ))

      structure(list(
        distr = distr6::Distribution$new(
          name = "Nonparametric Density",
          short_name = "NonparDens",
          type = set6::Reals$new(),
          pdf = pdf,
          parameters = ps),
        kernel = "Norm",
        bandwidth = bw
      ))
    },

    .predict = function(task) {
      list(
        pdf = self$model$distr$pdf(task$data()[[1]]),
        distr = self$model$distr)
    }
  )
)

.extralrns_dict$add("dens.nonpar", LearnerDensNonparametric)
