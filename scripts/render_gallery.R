

render_gallery <- function(category, metadata_file = NULL, path_prefix = "images") {
  if (!requireNamespace("glue", quietly = TRUE)) stop("Please install the 'glue' package")
  
  img_dir <- file.path(path_prefix, category)
  images <- list.files(img_dir, pattern = "\\.(jpg|jpeg|png|webp)$", ignore.case = TRUE)
  
  if (length(images) == 0) {
    cat(glue::glue("<p>No images found in {img_dir}</p>"))
    return(invisible(NULL))
  }
  
  metadata <- NULL
  if (!is.null(metadata_file) && file.exists(metadata_file)) {
    metadata <- readr::read_csv(metadata_file, show_col_types = FALSE)
  }
  
  cat("<div class='masonry'>\n")
  for (img in images) {
    row <- metadata[metadata$file == img, ]
    desc <- glue::glue("<strong>{row$title}</strong><br>{row$location} — {row$date}<br><em>{row$caption}</em>")
    full_path <- file.path(path_prefix, category, img)
    
    cat(glue::glue(
      "<a href='{full_path}' data-fancybox='{category}' data-caption=\"{desc}\">
         <img src='{full_path}' alt='{row$title}' />
       </a>\n"
    ))
  }
  cat("</div>\n")
}
