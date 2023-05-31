#' Recursive Feature Elimination
#'
#' Perform recursive feature elimination.
#'
#' Feature elimination can be sped up by computing the loss on a
#'  proportion ('prop') of the data, or by eliminating several ('k')
#'  features per step.
#'
#' @param x The independent variables (called features). Typically a matrix.
#' @param y The dependent variables. Typically a vector.
#' @param get_loss A loss function that takes the arguments 'x' and 'y' and
#'  returns a numeric value.
#' @param prop The proportion of samples used to compute loss. Provide a
#'  numeric value between 0 and 1.
#' @param k k The number of features eliminated each step. Provide an integer
#'  greater than 0.
#' @param verbose A boolean. Toggles whether to print progress.
#' @return A data.frame of ranked features.
#'
#' @export
rfe <-
  function(x,
           y,
           get_loss,
           prop = 1,
           k = 1,
           verbose = T) {
    # input member variables
    x = data.frame(x)
    y = data.frame(y)
    get_loss = get_loss
    prop = prop
    k = k
    verbose = verbose

    # pre-process input
    MAX_COLS = ncol(x)
    if (is.null(colnames(x))) {
      colnames(x) <- paste0("X", 1:ncol(x))
    }

    # derived member variables
    history_coef = NULL
    history = NULL
    current_loss = NULL

    sampler <-
      function() {
        if (!is.na(prop)) {
          index = sample(1:nrow(x), prop * nrow(x))
          current_x = x[index, , drop = F]
          current_y = y[index, , drop = F]
        } else{
          current_x = x
          current_y = y
        }
        list("x" = current_x, "y" = current_y)
      }

    deploy <-
      function() {
        current = sampler()
        perf = do.call("get_loss", list("x" = current$x, "y" = current$y))
        perf
      }

    test_elimination_loss <-
      function() {
        # Try eliminating every variable and see how model performs
        master_x = x
        NCOLS = ncol(master_x)
        perf_after_elim = vector("numeric", NCOLS)
        for (j in 1:NCOLS) {
          if (verbose)
            cat(j, "... ")
          x <<- master_x[,-j, drop = F] # eliminate a column
          perf_after_elim[j] = deploy()
        }
        if (verbose)
          cat("\n")
        x <<- master_x
        current_loss <<- perf_after_elim
      }

    eliminate_k_features <-
      function() {
        least_important = which_min(current_loss, k)
        history <<- c(history, colnames(x)[least_important])
        history_coef <<-
          c(history_coef, current_loss[least_important])
        x <<- x[,-least_important, drop = F]
      }

    main <-
      function() {
        # Initialize history with full model
        history_coef <<- deploy()

        while (ncol(x) > 0) {
          test_elimination_loss()
          eliminate_k_features()
        }

        # Remove NA loss from fitting model with no features
        history <<- rev(history)
        history_coef <<- rev(history_coef)[-1]
      }

    main()
    return(
      data.frame(
        "Step" = MAX_COLS:1,
        "Rank" = 1:MAX_COLS,
        "Column" = history,
        "Loss (Before Elim)" = history_coef
      )
    )
  }
