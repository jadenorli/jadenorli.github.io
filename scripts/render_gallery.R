

render_gallery <- function(category, shuffle = TRUE, path_prefix = "images") {
  # Load glue
  if (!requireNamespace("glue", quietly = TRUE)) stop("Please install the 'glue' package")
  
  # Build the full image directory path
  img_dir <- file.path(path_prefix, category)
  
  # List all image files
  images <- list.files(img_dir, pattern = "\\.(jpg|jpeg|png|webp)$", ignore.case = TRUE)
  
  if (length(images) == 0) {
    cat(glue::glue("<p>No images found in {img_dir}</p>"))
    return(invisible(NULL))
  }
  
  # Optionally shuffle the images
  if (shuffle) images <- sample(images)
  
  # Output the HTML
  cat("<div class='masonry'>\n")
  for (img in images) {
    # Alt text from filename (optional improvement: clean this up)
    alt <- tools::file_path_sans_ext(basename(img))
    cat(glue::glue("![{alt}]({file.path(path_prefix, category, img)})\n"))
  }
  cat("</div>\n")
}
