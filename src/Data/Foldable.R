list(
  foldrArray = function(f) {
    function(init) {
      function(xs) {
        acc <- init
        for (i in seq(along = xs)) {
          acc <- f(xs[[i]])(acc)
        }
        acc
      }
    }
  },
  foldlArray = function(f) {
    function(init) {
      function(xs) {
        acc <- init
        for (i in seq(along = xs)) {
          acc <- f(acc)(xs[[i]])
        }
        acc
      }
    }
  }
)
