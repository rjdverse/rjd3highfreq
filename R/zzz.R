#' @include utils.R
#' @import rjd3sts

.onLoad <- function(libname, pkgname) {
  if (!requireNamespace("rjd3sts", quietly = TRUE)) stop("Loading rjd3 libraries failed")

  result <- rJava::.jpackage(pkgname, lib.loc=libname)
  if (!result) stop("Loading java packages failed")

  #proto.dir <- system.file("proto", package = pkgname)
  #RProtoBuf::readProtoFiles2(protoPath = proto.dir)

  ## Add all jar files in the "inst/java" directory to the classpath as
  ## well. This is a kind of workaround for the source package and
  ## bundle package version of the project where jar files are in
  ## "inst/java" directory.
  ##
  ## The source and bundle package directory structure is used when two useful
  ## functions: `devtools::load_all()` and `devtools::test()` are executed.
  ## This workaround is not needed however when you run `devtools::check()` or
  ## `R CMD check` from command line or when you use this package from a
  ## third-party code when it is already installed in the system.
  ## It is not needed in these cases because the binary package directory
  ## structure is used.
  rJava::.jaddClassPath(dir(system.file("java", package=pkgname, lib.loc=libname), full.names = TRUE))



  # reload extractors
  .jcall("jdplus/toolkit/base/api/information/InformationExtractors", "V", "reloadExtractors")
}
