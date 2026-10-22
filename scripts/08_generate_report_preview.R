report_path <- path_for("report.md")
preview_path <- path_for("report_preview.md")
if (!file.exists(report_path)) {
  message("report.md is not present yet; preview generation will run after the report is written.")
} else {
  lines <- readLines(report_path, warn = FALSE, encoding = "UTF-8")
  image_lines <- grep("^【这里放：figures/[^】]+】$", lines, value = TRUE)
  for (placeholder in image_lines) {
    file <- sub("^【这里放：figures/([^】]+)】$", "\\1", placeholder)
    idx <- match(placeholder, lines)
    prior <- if (idx > 1L) lines[idx - 1L] else ""
    caption <- sub("^\\*\\*([^*]+)\\*\\*$", "\\1", prior)
    if (!nzchar(caption) || identical(caption, prior)) caption <- file
    lines[idx] <- paste0("![", caption, "](figures/", file, ")")
  }
  writeLines(lines, preview_path, useBytes = TRUE)
  message("Generated report_preview.md from report.md (", length(image_lines), " image links).")
}
