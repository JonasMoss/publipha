context("utility")

## alist2
expect_equal(alist2(x = y), alist(x = y))

## do_call
expect_equal(do.call(sum, as.list(1:10)), do_call(sum, as.list(1:10)))

## add_elements
ls <- list(a = 1, b = 2)

expect_equal(add_elements(ls, a = 3), ls)
expect_equal(add_elements(ls, c = 3), c(ls, c = 3))
expect_equal(
  add_elements(ls, c = d, .eager = FALSE),
  alist2(a = 1, b = 2, c = d)
)

## arguments
f <- function(...) arguments(expand_dots = TRUE)
expect_equal(f(1, 2, 3), as.list(1:3))
f <- function(a = 1, b = 2) arguments()
expect_equal(f(), list(NULL, NULL))
expect_equal(f(3, 6), list(a = 3, b = 6))
