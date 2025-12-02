#####
###
#     eda4ml_admin.R
#
#       Monitor the length and structure of the book.
###
#####

##
#   update_contents()
#
#     List tibbles (parts, chapters, sections)
## 
#' Update Table of Contents from _quarto.yml
#'
#' Reads the Quarto book configuration and chapter files to produce
#' three tibbles describing the book structure: parts, chapters, and sections.
#'
#' @param quarto_yml Path to _quarto.yml file (default: "_quarto.yml")
#' @param book_dir   Directory containing chapter .qmd files (default: NULL)
#'
#' @return A named list of three tibbles:
#'   - parts:    (idx_0, title)
#'   - chapters: (idx_0, idx_1, title, file, n_l)
#'   - sections: (idx_1, idx_2, title, l_0, l_1, n_l)
#'
#' @examples
#' \dontrun{
#'   contents <- update_contents("_quarto.yml", ".")
#'   contents$parts
#'   contents$chapters
#'   contents$sections
#' }

update_contents <- function(
    quarto_yml = "_quarto.yml", 
    book_dir = NULL
  ) {
  
  # ── Load required packages ───────────────────────────────────────────────
  require(dplyr)
  require(here)
  require(purrr)
  require(stringr)
  require(tibble)
  require(yaml)
  
  # ── Null-coalescing operator (if not already available) ──────────────────
  `%||%` <- function(x, y) if (is.null(x) || is.na(x)) y else x
  
  # ── Read and parse _quarto.yml ───────────────────────────────────────────
  if (is.null(book_dir)) {
    book_dir <- here::here()
  }
  fn_yml <- here::here(book_dir, quarto_yml)
  
  yml <- yaml::read_yaml(fn_yml)
  book_chapters <- yml$book$chapters
  
  # ── Initialize accumulators ──────────────────────────────────────────────
  parts_list    <- list()
  chapters_list <- list()
  sections_list <- list()
  
  part_idx    <- 0L
  chapter_idx <- 0L
  
  # ── Helper: extract sections from a .qmd file ────────────────────────────
  extract_sections <- function(file_path, chapter_idx) {
    if (!file.exists(file_path)) {
      warning(paste("File not found:", file_path))
      return(tibble(
        idx_1 = integer(),
        idx_2 = integer(),
        title = character(),
        l_0   = integer(),
        l_1   = integer(),
        n_l   = integer()
      ))
    }
    
    lines <- readLines(file_path, warn = FALSE)
    n_lines <- length(lines)
    
    # Find lines starting with "## " (level-2 headers = sections)
    section_pattern <- "^## "
    section_lines <- which(str_detect(lines, section_pattern))
    
    if (length(section_lines) == 0) {
      return(tibble(
        idx_1 = integer(),
        idx_2 = integer(),
        title = character(),
        l_0   = integer(),
        l_1   = integer(),
        n_l   = integer()
      ))
    }
    
    # Extract section titles (remove "## " and any trailing {#...})
    section_titles <- lines[section_lines] |>
      str_remove("^## ") |>
      str_remove("\\s*\\{.*\\}\\s*$") |>
      str_trim()
    
    # Calculate line ranges for each section
    n_sections <- length(section_lines)
    l_0 <- section_lines
    l_1 <- c(section_lines[-1] - 1L, n_lines)
    n_l <- l_1 - l_0 + 1L
    
    tibble(
      idx_1 = rep(chapter_idx, n_sections),
      idx_2 = seq_len(n_sections),
      title = section_titles,
      l_0   = l_0,
      l_1   = l_1,
      n_l   = n_l
    )
  }
  
  # ── Helper: extract chapter title from .qmd file ─────────────────────────
  extract_chapter_title <- function(file_path) {
    if (!file.exists(file_path)) {
      return(NA_character_)
    }
    
    lines <- readLines(file_path, warn = FALSE, n = 50)
    
    # Look for "# Title" pattern (level-1 header)
    title_line <- str_detect(lines, "^# ") |> which() |> head(1)
    
    if (length(title_line) == 0) {
      return(NA_character_)
    }
    
    lines[title_line] |>
      str_remove("^# ") |>
      str_remove("\\s*\\{.*\\}\\s*$") |>
      str_trim()
  }
  
  # ── Helper: count lines in a file ────────────────────────────────────────
  count_lines <- function(file_path) {
    if (!file.exists(file_path)) {
      return(NA_integer_)
    }
    length(readLines(file_path, warn = FALSE))
  }
  
  # ── Process book structure ────────────────────────────────────────────────
  
  # Add "Front Matter" as part 0
  parts_list[[1]] <- tibble(idx_0 = 0L, title = "Front Matter")
  
  # Track whether we've seen any parts yet (to distinguish front/back matter)
  seen_parts <- FALSE
  
  # Track the back matter part index (typically 6)
  back_matter_idx <- 6L
  
  for (item in book_chapters) {
    
    if (is.character(item)) {
      # ── Simple chapter file (front or back matter) ────────────────────────
      file_name <- item
      file_path <- file.path(book_dir, file_name)
      
      if (!seen_parts) {
        # Front matter item (before any part declaration)
        chapters_list[[length(chapters_list) + 1]] <- tibble(
          idx_0 = 0L,
          idx_1 = NA_integer_,
          title = "",
          file  = file_name,
          n_l   = count_lines(file_path)
        )
      } else {
        # Back matter item (appears after parts, e.g., summary.qmd, references.qmd)
        chapters_list[[length(chapters_list) + 1]] <- tibble(
          idx_0 = back_matter_idx,
          idx_1 = NA_integer_,
          title = extract_chapter_title(file_path) %||% "",
          file  = file_name,
          n_l   = count_lines(file_path)
        )
      }
      
    } else if (is.list(item) && !is.null(item$part)) {
      # ── Part declaration ──────────────────────────────────────────────────
      seen_parts <- TRUE
      part_title <- item$part
      
      # Check if this is the Appendices part (back matter)
      is_appendix <- str_detect(part_title, regex("appendix|appendices", ignore_case = TRUE))
      
      if (is_appendix) {
        # Appendices become "Back Matter" with idx_0 = 6
        # Add back matter part if not already added
        if (!any(sapply(parts_list, function(x) x$idx_0 == back_matter_idx))) {
          parts_list[[length(parts_list) + 1]] <- tibble(
            idx_0 = back_matter_idx, 
            title = "Back Matter"
          )
        }
        
        # Process appendix chapters (no idx_1, no sections)
        if (!is.null(item$chapters)) {
          for (chapter_file in item$chapters) {
            file_path <- file.path(book_dir, chapter_file)
            
            chapters_list[[length(chapters_list) + 1]] <- tibble(
              idx_0 = back_matter_idx,
              idx_1 = NA_integer_,
              title = extract_chapter_title(file_path) %||% "",
              file  = chapter_file,
              n_l   = count_lines(file_path)
            )
            # Note: sections not extracted for appendices
          }
        }
        
      } else {
        # Regular numbered part
        part_idx <- part_idx + 1L
        
        parts_list[[length(parts_list) + 1]] <- tibble(
          idx_0 = part_idx,
          title = part_title
        )
        
        # Process chapters within this part
        if (!is.null(item$chapters)) {
          for (chapter_file in item$chapters) {
            chapter_idx <- chapter_idx + 1L
            file_path <- file.path(book_dir, chapter_file)
            
            chapter_title <- extract_chapter_title(file_path)
            n_lines <- count_lines(file_path)
            
            chapters_list[[length(chapters_list) + 1]] <- tibble(
              idx_0 = part_idx,
              idx_1 = chapter_idx,
              title = chapter_title %||% "",
              file  = chapter_file,
              n_l   = n_lines
            )
            
            # Extract sections for this chapter
            sects <- extract_sections(file_path, chapter_idx)
            if (nrow(sects) > 0) {
              sections_list[[length(sections_list) + 1]] <- sects
            }
          }
        }
      }
    }
  }
  
  # Ensure Back Matter part exists if we added any back matter chapters
  has_back_matter <- any(sapply(chapters_list, function(x) {
    !is.null(x$idx_0) && x$idx_0 == back_matter_idx
  }))
  has_back_matter_part <- any(sapply(parts_list, function(x) {
    x$idx_0 == back_matter_idx
  }))
  
  if (has_back_matter && !has_back_matter_part) {
    parts_list[[length(parts_list) + 1]] <- tibble(
      idx_0 = back_matter_idx, 
      title = "Back Matter"
    )
  }
  
  # ── Combine results ───────────────────────────────────────────────────────
  parts_tbl <- bind_rows(parts_list)
  
  chapters_tbl <- bind_rows(chapters_list) |>
    mutate(
      idx_1 = as.integer(idx_1),
      n_l   = as.integer(n_l)
    )
  
  sections_tbl <- bind_rows(sections_list) |>
    mutate(across(c(idx_1, idx_2, l_0, l_1, n_l), as.integer))
  
  # ── Return named list ─────────────────────────────────────────────────────
  return(list(
    parts    = parts_tbl,
    chapters = chapters_tbl,
    sections = sections_tbl
  ))
}

##
#   write_contents()
#
#     Write tibbles (parts, chapters, sections) to TSV files
## 
#' Write contents tibbles to TSV files
#'
#' @param contents List returned by update_contents() (default: NULL)
#' @param out_dir  Output directory (default: NULL)
#' @param prefix   File name prefix (default: "")
#' @param suffix   File name suffix before .txt (default: "_eda4ml")
#'
#' @return list of file-paths to three written TSV (.txt) files

write_contents <- function(
    contents = NULL, 
    out_dir  = NULL, 
    prefix   = "", 
    suffix   = "_eda4ml"
) {
  require(here)
  require(readr)
  
  if (is.null(contents)) {
    contents <- update_contents()
  }
  
  if (is.null(out_dir)) {
    out_dir <- here::here("data", "retain")
  }
  
  fp_lst <- list(
    parts = 
      file.path( 
        out_dir, 
        paste0(prefix, "parts", suffix, ".txt") ), 
    chapters = 
      file.path( 
        out_dir, 
        paste0(prefix, "chapters", suffix, ".txt") ), 
    sections = 
      file.path( 
        out_dir, 
        paste0(prefix, "sections", suffix, ".txt") )
  )
  
  contents$ parts    |> readr::write_tsv( fp_lst [["parts"]] )
  contents$ chapters |> readr::write_tsv( fp_lst [["chapters"]] )
  contents$ sections |> readr::write_tsv( fp_lst [["sections"]] )
  
  return(fp_lst)
}


# ── Example usage ─────────────────────────────────────────────────────────
# contents <- update_contents("_quarto.yml", ".")
# write_contents(contents, out_dir = "data/retain")


##
#  EOF
##
