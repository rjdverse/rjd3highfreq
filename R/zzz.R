#' @import rjd3xjars

.onLoad <- function(libname, pkgname) {
    tryCatch({

        ## These checks are optional if the packages are in Imports.
        ## They are harmless if you want an explicit error message.
        if (!requireNamespace("rjd3toolkit", quietly = TRUE)) {
            stop(
                "Package 'rjd3toolkit' is required but not available.",
                call. = FALSE
            )
        }

        if (!requireNamespace("rjd3xjars", quietly = TRUE)) {
            stop(
                "Package 'rjd3xjars' is required but not available.",
                call. = FALSE
            )
        }

        # Loading Java class
        inst_dir <- file.path(libname, pkgname, "inst", "java")
        java_dir <- system.file("java", package = pkgname, lib.loc = libname)

        jars_files <- list.files(
            c(java_dir, inst_dir),
            pattern = "\\.jar$",
            full.names = TRUE,
            all.files = TRUE
        )

        ## Add Java files from the installed package
        if (length(jars_files) > 0L) {
            rJava::.jaddClassPath(jars_files)
        }

        ## Initialize the Java package
        result <- suppressMessages(
            suppressWarnings(
                rJava::.jpackage(
                    pkgname,
                    lib.loc = libname,
                    morePaths = jars_files
                )
            )
        )
        if (!isTRUE(result)) {
            stop("Loading Java packages failed", call. = FALSE)
        }

        ## Reload extractors
        try(
            suppressMessages(
                rJava::.jcall(
                    "jdplus/toolkit/base/api/information/InformationExtractors",
                    "V",
                    "reloadExtractors"
                )
            ),
            silent = TRUE
        )

    }, error = function(e) {
        stop(
            "Failed to load package: ",
            conditionMessage(e),
            call. = FALSE
        )
    })
}
