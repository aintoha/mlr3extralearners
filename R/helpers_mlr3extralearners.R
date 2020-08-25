toproper = function(str, split = " ", fixed = TRUE) {
  str = strsplit(str, split, fixed)
  str = lapply(str, function(x) {
    paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, 1000)), collapse = split)
  })
  return(unlist(str))
}

# a less robust but faster version of devtools::as.package
pkg_root = function(path = ".") {
  path = normalizePath(path)

  if (!grepl("mlr3extralearners", path)) {
    stopf("Path (%s) is not in mlr3extralearners directory.")
  }

  while (TRUE) {
    if (file.exists(file.path(path, "DESCRIPTION"))) {
      return(path)
    } else {
      path = dirname(path)
    }
  }
}

create_learner = function(pkg = ".", classname, algorithm, type, key = tolower(classname),
                          package = tolower(classname), caller,
                          feature_types, predict_types, properties = NULL,
                          importance = FALSE, oob_error = FALSE, references = FALSE,
                          gh_name) {

  path = pkg_root(pkg)

  checkmate::assert_choice(type, names(mlr3::mlr_reflections$task_properties))
  Type = toproper(type)

  checkmate::assert_character(key, len = 1)
  if (paste(type, key, sep = ".") %in% names(mlr_learners$items)) {
    stopf(
      "%s already exists in learner dictionary, please choose a different key.",
      paste(type, key, sep = "."))
  }

  algorithm = toproper(checkmate::assert_character(algorithm, len = 1))
  classname = toproper(checkmate::assert_character(classname, len = 1))
  checkmate::assert_character(caller, len = 1)
  checkmate::assert_character(gh_name, len = 1)

  checkmate::assert_subset(feature_types, unname(mlr3::mlr_reflections$task_feature_types))
  checkmate::assert_subset(predict_types,
                           names(mlr3::mlr_reflections$learner_predict_types[[type]]))
  checkmate::assert_subset(properties, mlr3::mlr_reflections$learner_properties[[type]])
  checkmate::assert_flag(importance)
  checkmate::assert_flag(oob_error)
  checkmate::assert_flag(references)

  type_lng = switch(type,
                    classif = "Classification",
                    regr = "Regression",
                    surv = "Survival",
                    dens = "Density")

  # ADD LEARNER
  file_name_lrn = file.path(path, "R", paste0("learner_", package, "_", type, "_", key, ".R"))
  x = file.copy(file.path(path, "templates", "learner_template.R"),
                to = file_name_lrn, overwrite = FALSE)
  add_str = c()
  if (!x) {
    mlr3misc::warningf("File %s already exists. Manually edit the file.", file_name_lrn)
  } else {
    mlr3misc::catf("Creating %s from template.\n", paste(type, key, sep = "_"))
    x = readLines(file_name_lrn)
    x = gsub("<algorithm>", algorithm, x)
    x = gsub("<Type_lng>", type_lng, x)
    x = gsub("<type>", type, x)
    x = gsub("<Type>", Type, x)
    x = gsub("<key>", key, x)
    x = gsub("<package>", package, x)
    x = gsub("<caller>", caller, x)
    x = gsub("<Classname>", classname, x)
    x = gsub("<gh_name>", gh_name, x)
    x = gsub("<feature_types>", paste0(feature_types, collapse = '", "'), x)
    x = gsub("<predict_types>", paste0(predict_types, collapse = '", "'), x)
    if (length(properties)) {
      x = gsub("<properties>", paste0(properties, collapse = '", "'), x)
    } else {
      x = x[-which(grepl("<properties>", x))]
    }
    # fix commas
    if (importance & !oob_error) {
      x[which(grepl("importance = function()", x))] = "    importance = function() { }"
    } else if (!importance & !oob_error) {
      x[which(grepl("man =", x)) + 2] = "    }"
    }
    if (!importance) {
      x = x[-seq.int(
        which(grepl("FIXME - ADD IMPORTANCE METHOD HERE", x)),
        which(grepl("importance = function()", x)))]
    } else {
      add_str = c(add_str, "importance method")
    }
    if (!oob_error) {
      x = x[-seq.int(
        which(grepl("FIXME - ADD OOB_ERROR METHOD HERE", x)),
        which(grepl("oob_error = function()", x)))]
    } else {
      add_str = c(add_str, "oob_error method")
    }
    if (!references) {
      x = x[-seq.int(which(grepl("@references", x)), which(grepl("@references", x)) + 1)]
    } else {
      add_str = c(add_str, "references")
    }
    cat(x, file = file_name_lrn, sep = "\n")
  }


  # ADD TESTS
  file_name_test = file.path(path, "tests", "testthat", paste0("test_", package, "_", type, "_",
                                                               key, ".R"))
  x = file.copy(file.path(path, "templates", "test_template.R"), to = file_name_test,
                overwrite = FALSE)
  if (!x) {
    mlr3misc::warningf("File %s already exists. Manually edit the file.", file_name_test)
  } else {
    mlr3misc::catf("Creating %s tests from template.\n", paste(type, key, sep = "_"))
    x = readLines(file_name_test)
    x = gsub("<type>", type, x)
    x = gsub("<Type>", Type, x)
    x = gsub("<key>", key, x)
    x = gsub("<Classname>", classname, x)
    cat(x, file = file_name_test, sep = "\n")
  }

  # ADD PARAM TESTS
  file_name_ptest = file.path(path, "inst", "paramtest", paste0("test_paramtest_", package, "_",
                                                                type, "_", key, ".R"))
  x = file.copy(file.path(path, "templates", "param_test_template.R"), to = file_name_ptest,
                overwrite = FALSE)
  if (!x) {
    mlr3misc::warningf("File %s already exists. Manually edit the file.", file_name_ptest)
  } else {
    mlr3misc::catf("Creating %s paramtests from template.\n", paste(type, key, sep = "_"))
    x = readLines(file_name_ptest)
    x = gsub("<type>", type, x)
    x = gsub("<key>", key, x)
    x = gsub("<package>", package, x)
    x = gsub("<caller>", caller, x)
    cat(x, file = file_name_ptest, sep = "\n")
  }


  # CREATE YAMLS
  file_name = file.path(path, ".github", "workflows", paste0("test_", key, ".yml"))
  x = file.copy(file.path(path, "templates", "test_template.yml"), to = file_name,
                overwrite = FALSE)
  if (!x) {
    messagef("Learner test YAML for {%s} already exists.", package)
  } else {
    mlr3misc::catf("Creating {%s} learner test YAML file from template.\n", package)
    x = readLines(file_name)
    x = gsub("<package>", package, x)
    cat(x, file = file_name, sep = "\n")
  }

  # UPDATE DESCRIPTION
  x = readLines(file.path(path, "DESCRIPTION"))
  if (!any(grepl(package, x))) {
    mlr3misc::catf("Adding %s to DESCRIPTION Suggests.\n\n", package)
    x = gsub("testthat", paste0(c("testthat", package), collapse = ",\n    "), x)
    cat(x, file = file.path(path, "DESCRIPTION"), sep = "\n")
  } else {
    messagef("{%s} already exists in DESCRIPTION.", package)
  }

  # UPDATE USER
  mlr3misc::catf(
    "Now manually do the following:
  1) For %s:
    a) Add .train and .predict private methods.
    b) Add param_set and if applicable param_vals.
    c) Check generated Learner file carefully.
    %s.
  2) For %s:
    a) Check tests pass once learner is complete.
    b) Optionally modify learner parameter values.
  3) For %s:
    a) Check tests pass once learner is complete.
    b) Optionally add further tests for all functions called in your .train and .predict methods.
  4) Run:
    a) devtools::document(roclets = c('rd', 'collate', 'namespace'))
    b) styler::style_pkg(style = styler::mlr_style)
    c) usethis::use_tidy_description()
    d) lintr::lint_package()
  5) Open a pull request to https://github.com/mlr-org/mlr3extralearners/pulls with the new learner template.", # nolint
    file_name_lrn, ifelse(length(add_str), paste("d) Add", paste0(add_str, collapse = ", ")), ""),
    file_name_test, file_name_ptest)


  # OPEN FILES
  cat(file_name_lrn)
  file.edit(c(file_name_lrn, file_name_test, file_name_ptest))
}

#' @title List Learners in mlr3verse
#' @description Lists all learners, properties, and associated packages in a table that can be
#' filtered and queried.
#' @param select `character()` \cr Passed to [data.table::subset].
#' @param filter `list()` \cr Named list of conditions to filter on, names correspond to column
#' names in table.
#' @examples
#' list_mlr3learners(
#'   select = c("id", "properties", "predict_types"),
#'   filter = list(class = "surv", predict_types = "distr"))
#' @export
list_mlr3learners = function(select = NULL, filter = NULL) {
  dt = copy(mlr3learners_table)

  if (!is.null(filter)) {
    if (!is.null(filter$name)) {
      dt = subset(dt, name %in% filter$name)
    }
    if (!is.null(filter$class)) {
      dt = subset(dt, class %in% filter$class)
    }
    if (!is.null(filter$id)) {
      dt = subset(dt, id %in% filter$id)
    }
    if (!is.null(filter$mlr3_package)) {
      dt = subset(dt, mlr3_package %in% filter$mlr3_package)
    }
    if (!is.null(filter$required_package)) {
      dt = subset(dt, required_package %in% filter$required_package)
    }
    if (!is.null(filter$properties)) {
      dt = subset(dt, mlr3misc::map_lgl(
        dt$properties,
        function(.x) any(filter$properties %in% .x)))
    }
    if (!is.null(filter$feature_types)) {
      dt = subset(dt, mlr3misc::map_lgl(
        dt$feature_types,
        function(.x) any(filter$feature_types %in% .x)))
    }
    if (!is.null(filter$predict_types)) {
      dt = subset(dt, mlr3misc::map_lgl(
        dt$predict_types,
        function(.x) any(filter$predict_types %in% .x)))
    }
  }

  if (!is.null(select)) {
    dt = subset(dt, select = select)
  }

  return(dt)
}

#' @title Install Learner Dependencies
#' @description Install required dependencies for specified learners.
#' @param .keys `character()` \cr Keys passed to [mlr_learners][mlr3::mlr_learners] specifying
#' learners to install.
#' @param ... `ANY` \cr Additional options to pass to [install.packages].
#' @examples
#' \dontrun{
#' install_learners(c("regr.gbm", "classif.kknn"))
#' }
#' @export
install_learners = function(.keys, ...) {
  sapply(.keys, function(.key) {
    pkgs = mlr3::lrn(.key)$packages
    sapply(pkgs, function(pkg) {
      x = tryCatch(find.package(pkg),
                   error = function(e) utils::install.packages(pkg, ...))
    })
  })
  invisible()
}