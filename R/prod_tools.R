
add_class = function(x, new_class=NULL) {
  class(x) = union(new_class, class(x))
  x
}
