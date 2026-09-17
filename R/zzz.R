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

        ## Initialize the Java package
        result <- suppressMessages(
            suppressWarnings(
                rJava::.jpackage(
                    pkgname,
                    lib.loc = libname
                )
            )
        )

        if (!isTRUE(result)) {
            stop(
                "Loading Java packages failed.",
                call. = FALSE
            )
        }

        ## Add Java files from the installed package
        java_dir <- system.file(
            "java",
            package = pkgname,
            lib.loc = libname
        )

        if (nzchar(java_dir) && dir.exists(java_dir)) {
            java_files <- list.files(
                java_dir,
                full.names = TRUE
            )

            if (length(java_files) > 0L) {
                rJava::.jaddClassPath(java_files)
            }
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
