#' Make A Checklist
#'
#' Make a checklist.
#'
#' In checklist, we define a checklist as a linear model with integer
#'  coefficients. When coefficients are integers, it is feasible for a human
#'  to calculate predictions "by hand". When the number of features are few,
#'  the model is easy to interpret and thus easy to implement in practice.
#'  We wrote this tool to make it easy to make a checklist from your data. The tool
#'  will calculate the integer coefficients and eliminate the unimportant features.
#'  Ideally, the features used in a checklist are binary,
#'  but it is up to you to make them binary.
#'
#' @inheritParams rfe
#' @param u A scale factor of weights assigned to each feature.
#'  Increasing this number will provide greater resolution,
#'  but less interpretability.
#'  Decreasing this number will provide less resolution,
#'  but greater interpretability.
#' @param simplify A boolean. Toggles whether to remove coefficients that
#'  have zero weight after being converted to an integer.
#'  Set TRUE by default.
#' @return A checklist object.
#'
#' @rdname checklist
#' @importFrom stats coefficients lm cor predict
#' @export
checklist <-
  function(x, y, u = 5, simplify = TRUE) {
    y = scale(y)
    m = lm(y ~ ., data.frame(x, y))
    pt = m_to_integer_coef(m, u)
    pt[is.na(pt)] = 0

    if (simplify) {
      pt = pt[pt != 0]
    }

    res = list(x = x,
               y = y,
               m = m,
               points = pt)
    class(res) = "checklist"
    return(res)
  }

#' @rdname checklist
#' @param object A checklist object.
#' @param ... Ignored.
#' @export
predict.checklist <-
  function(object, x, ...) {
    if (is.null(colnames(x))) {
      colnames(x) <- paste0("X", 1:ncol(x))
    }

    TRAIN_COLS <- names(object$points)
    if (!all(TRAIN_COLS %in% colnames(x))) {
      stop("Columns not found in new data.")
    }

    x_wt <-
      sweep(x[, TRAIN_COLS, drop = FALSE], 2, object$points, "*") # multiply each row by the weight
    yhat <- rowSums(x_wt)
    return(yhat)
  }

#' @rdname checklist
#' @export
make_checklist_loss <-
  function(u) {
    return(function(x, y) {
      chk = checklist(x, y, u)
      yhat = predict(chk, x)
      1 - cor(yhat, y, method = "spearman")
    })
  }

m_to_integer_coef <-
  function(m, u) {
    coef = coefficients(m)
    pt = round(u * coef)
    pt = pt[names(pt) != "(Intercept)"]
    pt
  }
