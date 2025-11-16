# scripts/render_gallery.R

render_gallery <- function(category,
                           metadata_file = NULL,
                           path_prefix   = "images",   # where your originals live
                           output_dir    = "docs",     # web root used by GitHub Pages
                           randomize_mode = c("always","daily","off"),
                           exts = c("jpg","jpeg","png","webp","gif","avif","tif","tiff")) {
  
  randomize_mode <- match.arg(randomize_mode)
  
  # ---- helpers -------------------------------------------------------------
  # Normalize to UTF-8 + NFC (avoid macOS NFD surprises)
  nfc <- function(x) {
    stringi::stri_trans_nfc(iconv(as.character(x), "", "UTF-8", sub = ""))
  }
  
  # Escape for HTML attributes
  esc_attr <- function(x) {
    x <- nfc(x)
    x <- gsub("&",  "&amp;",  x, fixed = TRUE)
    x <- gsub("<",  "&lt;",   x, fixed = TRUE)
    x <- gsub(">",  "&gt;",   x, fixed = TRUE)
    x <- gsub("\"", "&quot;", x, fixed = TRUE)
    x <- gsub("'",  "&#39;",  x, fixed = TRUE)
    x
  }
  
  # Build a *web-safe* filename from an original name
  # (diacritics -> ASCII, punctuation -> -)
  safe_name <- function(fname) {
    ext  <- tolower(tools::file_ext(fname))
    base <- tools::file_path_sans_ext(basename(fname))
    
    base <- stringi::stri_trans_nfc(base)
    base <- stringi::stri_trans_general(base, "Latin-ASCII")  # é → e, ñ → n, …
    base <- gsub("[’']", "", base)                            # drop apostrophes/quotes
    base <- gsub("[^A-Za-z0-9]+", "-", base)
    base <- gsub("-+", "-", base)
    base <- gsub("(^-|-$)", "", base)
    
    paste0(tolower(base), ".", ext)
  }
  
  # Join path but keep slashes; percent-encode unsafe chars segment-wise
  url_join <- function(...) {
    segs <- lapply(list(...), function(s) {
      s <- gsub("\\\\", "/", s)
      parts <- strsplit(s, "/", fixed = TRUE)[[1]]
      paste(vapply(parts, utils::URLencode, "", reserved = TRUE), collapse = "/")
    })
    paste(segs, collapse = "/")
  }
  
  # ---- find originals ------------------------------------------------------
  img_dir   <- file.path(path_prefix, category)  # e.g. "images/terrestrial"
  pat       <- paste0("\\.(", paste(exts, collapse = "|"), ")$", collapse = "")
  originals <- sort(list.files(img_dir, pattern = pat, ignore.case = TRUE))
  
  if (!length(originals)) {
    warning("No images found in ", img_dir)
    return(knitr::asis_output("<div class='masonry'></div>\n"))
  }
  
  # ---- read metadata (robust UTF-8) ---------------------------------------
  meta <- NULL
  if (!is.null(metadata_file) && file.exists(metadata_file)) {
    # try to detect encoding, then normalize to NFC
    enc <- "UTF-8"
    ge  <- try(readr::guess_encoding(metadata_file, n_max = 2000), silent = TRUE)
    if (!inherits(ge, "try-error") && nrow(ge) && !is.na(ge$encoding[1])) {
      enc <- ge$encoding[1]
    }
    
    meta <- readr::read_csv(
      metadata_file,
      locale = readr::locale(encoding = enc),
      show_col_types = FALSE
    )
    
    stopifnot("file" %in% names(meta))
    
    for (nm in c("file", "title", "location", "date", "caption")) {
      if (!nm %in% names(meta)) meta[[nm]] <- ""
      meta[[nm]] <- nfc(meta[[nm]])
    }
    
    # normalize matching keys: lower, NFC, ignore extension
    meta$.key <- tolower(tools::file_path_sans_ext(meta$file))
  }
  
  # ---- ensure copies exist under docs/ with safe names ---------------------
  dest_dir <- file.path(output_dir, path_prefix, category)  # docs/images/terrestrial
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # map original -> web-safe copy
  map <- lapply(originals, function(f) {
    src <- file.path(img_dir, f)
    web <- safe_name(f)                     # e.g., "sao-tome-osprey-reef.jpg"
    dst <- file.path(dest_dir, web)
    
    if (!file.exists(dst) || file.info(src)$mtime > file.info(dst)$mtime) {
      ok <- try(file.copy(src, dst, overwrite = TRUE), silent = TRUE)
      if (inherits(ok, "try-error") || !isTRUE(ok)) {
        warning("Could not copy ", src, " -> ", dst)
      }
    }
    
    list(orig = f, web = web)
  })
  
  map <- do.call(rbind, lapply(map, as.data.frame))
  names(map) <- c("orig", "web")
  map$orig_key <- tolower(tools::file_path_sans_ext(nfc(map$orig)))
  
  # ---- build HTML ----------------------------------------------------------
  classes   <- paste("masonry", if (randomize_mode != "off") "randomize-on-load")
  data_attr <- if (randomize_mode != "off") {
    paste0(" data-randomize=\"", randomize_mode, "\"")
  } else {
    ""
  }
  
  out <- sprintf("<div class='%s'%s>\n", classes, data_attr)
  
  for (i in seq_len(nrow(map))) {
    img_orig <- nfc(map$orig[i])
    img_web  <- nfc(map$web[i])
    
    # find metadata row by key (case/diacritics insensitive, ignore extension)
    row <- NULL
    if (!is.null(meta)) {
      row <- meta[meta$.key == map$orig_key[i], , drop = FALSE]
      if (!nrow(row)) row <- NULL
    }
    
    cap <- loc <- dat <- ""
    if (!is.null(row)) {
      cap <- esc_attr(row$caption[1])
      loc <- esc_attr(row$location[1])
      dat <- esc_attr(row$date[1])
    }
    
    parts <- character(0)
    if (nzchar(cap)) parts <- c(parts, sprintf("<strong>%s</strong>", cap))
    if (nzchar(loc)) parts <- c(parts, loc)
    if (nzchar(dat)) parts <- c(parts, sprintf("<em>%s</em>", dat))
    desc <- nfc(paste(parts, collapse = "<br>"))
    
    # URL relative to site root (docs/)
    rel_url <- url_join(path_prefix, category, img_web)
    alt_txt <- if (nzchar(cap)) cap else tools::file_path_sans_ext(img_orig)
    
    out <- paste0(
      out,
      "<a href='", rel_url, "' data-fancybox='", esc_attr(category),
      "' data-caption=\"", desc, "\">",
      "<img src='", rel_url, "' alt='", esc_attr(alt_txt),
      "' loading='lazy' decoding='async'>",
      "</a>\n"
    )
  }
  
  out <- paste0(out, "</div>\n")
  Encoding(out) <- "UTF-8"
  knitr::asis_output(out)
}
