if (!exists(".project_root", envir = .GlobalEnv, inherits = FALSE)) {
  stop("Run scripts/run_all.R so that the project root is resolved first.")
}
project_root <- get(".project_root", envir = .GlobalEnv)
dir.create(file.path(project_root, ".Rlib"), recursive = TRUE, showWarnings = FALSE)
.libPaths(c(file.path(project_root, ".Rlib"), .libPaths()))

required_packages <- c("forecast", "ggplot2", "urca")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace,
                                               logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  message("Attempting CRAN installation into the project library: ",
          paste(missing_packages, collapse = ", "))
  tryCatch(utils::install.packages(missing_packages, lib = file.path(project_root, ".Rlib"),
                                   repos = "https://cloud.r-project.org"),
           error = function(e) stop("Package installation failed: ", conditionMessage(e)))
}
missing_packages <- required_packages[!vapply(required_packages, requireNamespace,
                                               logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop("Required packages unavailable: ", paste(missing_packages, collapse = ", "))
}

options(stringsAsFactors = FALSE, scipen = 6, digits = 7)
try(Sys.setlocale("LC_CTYPE", "Chinese (Simplified)_China.936"), silent = TRUE)
if (.Platform$OS.type == "windows") {
  try(grDevices::windowsFonts(yahei = grDevices::windowsFont("Microsoft YaHei")),
      silent = TRUE)
}
ggplot2::theme_set(ggplot2::theme_minimal(base_size = 12,
                                           base_family = "Microsoft YaHei") +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 15),
                 plot.subtitle = ggplot2::element_text(size = 10, colour = "#52616B"),
                 axis.title = ggplot2::element_text(size = 11),
                 axis.text = ggplot2::element_text(colour = "#263238"),
                 panel.grid.minor = ggplot2::element_blank(),
                 legend.position = "bottom", legend.title = ggplot2::element_blank()))

path_for <- function(...) file.path(project_root, ...)
write_csv <- function(x, ...) utils::write.csv(x, path_for(...), row.names = FALSE,
                                                fileEncoding = "UTF-8", na = "")
save_plot <- function(plot, filename, width = 9, height = 5.4) {
  ggplot2::ggsave(path_for("figures", filename), plot = plot, width = width,
                  height = height, units = "in", dpi = 300, bg = "white",
                  device = grDevices::png, type = "cairo",
                  family = "Microsoft YaHei", limitsize = FALSE)
  if (interactive()) print(plot)
}
message("Project root: ", normalizePath(project_root, winslash = "/", mustWork = TRUE))
message("R version: ", R.version.string)
for (pkg in required_packages) message("Package ", pkg, " ", utils::packageVersion(pkg))
