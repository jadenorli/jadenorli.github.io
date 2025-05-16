




if (!requireNamespace("fs", quietly = TRUE)) install.packages("fs")

generate_metadata <- function(category, path_prefix = "images", output_dir = "data") {
  img_dir <- fs::path(path_prefix, category)
  img_files <- list.files(img_dir, pattern = "\\.(jpg|jpeg|png|webp)$", ignore.case = TRUE)
  
  if (length(img_files) == 0) {
    stop("No image files found in ", img_dir)
  }
  
  metadata <- data.frame(
    file = img_files,
    title = tools::file_path_sans_ext(img_files),
    location = "",
    date = "",
    caption = "",
    stringsAsFactors = FALSE
  )
  
  if (!dir.exists(output_dir)) dir.create(output_dir)
  output_file <- file.path(output_dir, paste0(category, "_metadata.csv"))
  write.csv(metadata, output_file, row.names = FALSE)
  message("Metadata saved to: ", output_file)
}
