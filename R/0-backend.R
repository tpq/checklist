which_max <-
  function(x, N = 1) {
    if (length(x) < N) {
      N = length(x)
    }
    order(x, decreasing = T)[1:N]
  }

which_min <-
  function(x, N = 1) {
    if (length(x) < N) {
      N = length(x)
    }
    order(x, decreasing = F)[1:N]
  }
