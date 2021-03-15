pkg_fit_dir <- system.file("fit", package = "dualR")

tmp_fit_dir <- file.path(tempdir(check = TRUE), "fit")

unlink(tmp_fit_dir, recursive = TRUE, force = TRUE)

dir.create(tmp_fit_dir, showWarnings = FALSE, recursive = TRUE)

file.copy(pkg_fit_dir, tmp_fit_dir, recursive = TRUE)
