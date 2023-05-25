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
#' @return A checklist object.
#'
#' @rdname checklist
#' @importFrom stats coefficients lm
#' @export
checklist <-
  function(x, y, u = 5) {
    y = scale(y)
    m = lm(y ~ ., data.frame(x, y))
    pt = m_to_integer_coef(m, u)

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
    x_wt <-
      sweep(x, 2, object$points, "*") # multiply each row by the weight
    yhat <- rowSums(x_wt)
    return(yhat)
  }

m_to_integer_coef <-
  function(m, u) {
    coef = coefficients(m)
    pt = round(u * coef)
    pt = pt[names(pt) != "(Intercept)"]
    pt
  }
