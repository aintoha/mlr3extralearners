#' @title Density Kerdiest Kernel Learner
#' @author RaphaelS1
#' @name mlr_learners_dens.kde_kd
#' @template class_learner
#' @templateVar id dens.kde_kd
#' @templateVar caller kde
#'
#' @template seealso_learner
#' @template example
#' @export
LearnerDensKDEkd = R6Class("LearnerDensKDEkd",
  inherit = mlr3proba::LearnerDens,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(
        params = list(
          ParamDbl$new(id = "bw", lower = 0, tags = "train"),
          ParamFct$new("type_kernel",
            levels = c("n", "e", "t", "b"),
            default = "n", tags = "train")
        )
      )

      super$initialize(
        id = "dens.kde_kd",
        packages = "kerdiest",
        feature_types = c("integer", "numeric"),
        predict_types = c("pdf", "distr"),
        param_set = ps,
        man = "mlr3extralearners::mlr_learners_dens.kde_kd"
      )
    }
  ),

  private = list(
    .train = function(task) {

      pars = self$param_set$get_values(tag = "train")

      data = task$data()[[1]]

      pdf <- function(x) {
      }
      body(pdf) <- substitute({
        mlr3misc::invoke(kerdiest::kde, vec_data = data, y = x, .args = pars)$Estimated_values
      })

      if (is.null(pars$type_kernel)) {
        kernel = "Norm"
      } else {
        kernel = switch(pars$type_kernel,
          "n" = "Norm",
          "e" = "Epan",
          "b" = "Quart",
          "t" = "Triw")
      }

      bw = kerdiest::CVbw(vec_data = data)$bw

      if (is.null(pars$bw)) {
        bw = kerdiest::CVbw(vec_data = data)$bw
      } else {
        bw = pars$bw
      }

      ps = distr6::ParameterSet$new(
        id = list("bandwidth", "kernel"),
        value = list(bw, kernel),
        support = list(
          set6::Reals$new(),
          set6::Set$new(elements = as.list(distr6::listKernels()[, 1]))
      ))


      structure(list(
        distr = distr6::Distribution$new(
          name = paste("kerdiest KDE"),
          short_name = paste0("kerdiestKDEKern_"),
          pdf = pdf, type = set6::Reals$new(), parameters = ps),
        bandwidth = bw,
        kernel = kernel
      ))
    },

    .predict = function(task) {
      list(
        pdf = self$model$distr$pdf(task$data()[[1]]),
        distr = self$model$distr)
    }
  )
)

.extralrns_dict$add("dens.kde_kd", LearnerDensKDEkd)
