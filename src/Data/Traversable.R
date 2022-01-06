list(
  traverseArrayImpl = (function() {
    array1 <- function(a) list(a)
    array2 <- function(a) function(b) list(a, b)
    array3 <- function(a) function(b) function(d) list(a, b, d)
    concat2 <- function(xs) function(ys) c(xs, ys)

    function(apply) {
      function(map) {
        function(pure) {
          function(f) {
            function(array) {
              go <- function(bot, top) {
                d <- top - bot
                if (d == 0) {
                  return(pure(list()))
                } else if (d == 1) {
                  return(map(array1)(f(array[[bot + 1]])))
                } else if (d == 2) {
                  return(apply(map(array2)(f(array[[bot + 1]])))(f(array[[bot + 2]])))
                } else if (d == 3) {
                  return(apply(apply(map(array3)(f(array[[bot + 1]])))(f(array[[bot + 2]])))(f(array[[bot + 3]])))
                } else {
                  pivot <- bot + floor((top - bot) / 4) * 2
                  apply(map(concat2)(go(bot, pivot)))(go(pivot, top))
                }
              }
              go(0, length(array))
            }
          }
        }
      }
    }
  })()
)
