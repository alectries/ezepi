.onLoad <- function(libname, pkgname){
  # Explicitly import the pipe
  pkg_env <- new.env(parent = parent.env(environment()))
  pkg_env$`%>%` <- magrittr::`%>%`
  attach(pkg_env, name = paste0("package:", pkgname), warn.conflicts = FALSE)

}
