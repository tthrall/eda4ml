#####
###
#     lg_schema.R
#
#       Formal schema definition for LearningGraph, 
#       part of the eda4ml project.
#
#       The schema defines the vocabulary of permissible 
#       node types, edge types, and their constraints.
#       This enables both validation and documentation.
###
#####

##
#   lg_schema()
##
#' Define the LearningGraph Schema
#'
#' Returns a list containing the formal schema specification for LearningGraph
#' objects, including node types, edge types, constraints, and proficiency
#' level definitions.
#'
#' @return A list with components: node_types, edge_types, proficiency_levels
#' @export
#'
#' @examples
#' schema <- lg_schema()
#' schema$edge_types
lg_schema <- function() {
  
  # Schema metadata
  metadata = list(
    name = "LearningGraph Schema",
    version = "1.0.0",
    description = "Formal schema for skills-based learning knowledge graphs"
  )
  
  # Node type definitions
  node_types = tibble::tibble(
    type = c(
      "learner", "skill", "work_role", "course", "competency" ),
    description = c(
      "An individual acquiring skills",
      "A discrete capability that can be learned and assessed",
      "A job function with defined skill requirements",
      "A learning experience that develops skills",
      "A thematic grouping of related skills" ),
    id_field = c(
      "learner_id", "skill_id", "role_id", "course_id", "cmp_id" ),
    name_field = c(
      "name", "skill_name", "role_name", "course_name", "cmp_name" )
  )
  
  # Edge type definitions with constraints
  edge_types = tibble::tibble(
    edge_type = c(
      "has_skill",
      "requires_skill",
      "prerequisite",
      "teaches",
      "skill_in_competency"
    ),
    source_type = c(
      "learner", "work_role", "skill", "course", "skill" ),
    target_type = c(
      "skill", "skill", "skill", "skill", "competency" ),
    property = c(
      "proficiency", "proficiency", NA_character_, "proficiency", NA_character_ ),
    property_semantics = c(
      "current_level",      # where the learner stands now
      "minimum_threshold",  # minimum acceptable level for role
      NA_character_,        # no proficiency—conceptual dependency
      "maximum_ceiling",    # highest level course can develop
      NA_character_         # membership, no level
    ),
    description = c(
      "Learner's demonstrated proficiency in a skill",
      "Minimum proficiency required for a work role",
      "Conceptual dependency between skills (no proficiency)",
      "Maximum proficiency level a course can develop",
      "Skill belongs to a competency area" )
  )
  
  # Proficiency level definitions
  proficiency_levels = tibble::tibble(
    level = 0L:4L,
    label = c(
      "None", "Basic", "Intermediate", "Advanced", "Master" ),
    guidance_needed = c(
      "N/A", "Frequently", "Occasionally", "Rarely", "None" ),
    can_teach_others = c(FALSE, FALSE, FALSE, TRUE, TRUE),
    description = c(
      "No demonstrated competency",
      "Can perform routine tasks with guidance",
      "Can handle non-routine situations with occasional guidance",
      "Can handle complex situations independently",
      "Can advance the field and mentor others" )
  )
  
  return(list(
    metadata           = metadata, 
    node_types         = node_types, 
    edge_types         = edge_types, 
    proficiency_levels = proficiency_levels
  ))
}


##
#   lg_validate()
##
#' Validate a LearningGraph Against the Schema
#'
#' Checks that a LearningGraph object conforms to the schema constraints,
#' reporting any violations.
#'
#' @param lg A LearningGraph object (list with nodes and edges components)
#' @param schema Optional schema object; uses lg_schema() if not provided
#' @param verbose Logical; if TRUE, prints detailed validation messages
#'
#' @return A list with components: valid (logical), errors (character vector),
#'   warnings (character vector)
#' @export
#'
#' @examples
#' lg <- create_learning_graph()
#' result <- lg_validate(lg)
#' result$valid
lg_validate <- function(
    lg, schema = NULL, verbose = TRUE
) {
  
  if (is.null(schema)) {
    schema <- lg_schema()
  }
  
  errors   <- character()
  warnings <- character()
  
  # Helper to add error
  add_error <- function(msg) {
    errors <<- c(errors, msg)
  }
  
  # Helper to add warning
  add_warning <- function(msg) {
    warnings <<- c(warnings, msg)
  }
  
  # --- Structural checks ---
  if ( !"nodes" %in% names(lg) ) {
    add_error("Missing 'nodes' component")
  }
  if ( !"edges" %in% names(lg) ) {
    add_error("Missing 'edges' component")
  }
  
  # If structure is missing, return early
  if ( length(errors) > 0)  {
    return(list(
      valid    = FALSE, 
      errors   = errors, 
      warnings = warnings
    ))
  }
  
  # --- Node type checks ---
  expected_node_collections <- c(
    "competencies", "skills", "work_roles", "courses", "learners" )
  
  actual_node_collections <- names(lg$ nodes)
  
  missing_nodes <- setdiff(
    expected_node_collections, 
    actual_node_collections )
  if ( length(missing_nodes) > 0 ) {
    add_error(paste(
      "Missing node collections:", 
      paste(missing_nodes, collapse = ", ")
    ))
  }
  
  extra_nodes <- setdiff(
    actual_node_collections, 
    expected_node_collections )
  if (length(extra_nodes) > 0) {
    add_warning(paste(
      "Unexpected node collections:", 
      paste(extra_nodes, collapse = ", ") )
    )
  }
  
  # --- Edge type checks ---
  expected_edge_types <- schema$ edge_types$ edge_type
  actual_edge_types   <- names(lg$ edges)
  
  missing_edges <- setdiff(
    expected_edge_types, 
    actual_edge_types )
  if ( length(missing_edges ) > 0 ) {
    add_error(paste(
      "Missing edge types:", 
      paste(missing_edges, collapse = ", ") )
    )
  }
  
  extra_edges <- setdiff(
    actual_edge_types, 
    expected_edge_types )
  if ( length(extra_edges) > 0 ) {
    add_warning(paste(
      "Unexpected edge types:", 
      paste(extra_edges, collapse = ", ") )
    )
  }
  
  # --- Proficiency range checks ---
  valid_levels <- schema$ proficiency_levels$ level
  
  # Check has_skill proficiency values
  if ("has_skill" %in% names(lg$ edges)) {
    hs_levels <- lg$ edges$ has_skill$ proficiency
    invalid_hs <- hs_levels[ !hs_levels %in% valid_levels ]
    if ( length(invalid_hs) > 0 ) {
      add_error(paste(
        "Invalid proficiency levels in has_skill:", 
        paste(unique(invalid_hs), collapse = ", ") ) 
      )
    }
  }
  
  # Check requires_skill proficiency values
  if ("requires_skill" %in% names(lg$ edges)) {
    # Handle both possible column names
    rs_df <- lg$ edges$ requires_skill
    if ("required_proficiency" %in% names(rs_df)) {
      rs_levels <- rs_df$ required_proficiency
    } else if ("proficiency" %in% names(rs_df)) {
      rs_levels <- rs_df$ proficiency
    } else {
      rs_levels <- integer()
      add_warning("requires_skill missing proficiency column")
    }
    invalid_rs <- rs_levels[ !rs_levels %in% valid_levels ]
    if ( length(invalid_rs) > 0 ) {
      add_error(paste(
        "Invalid proficiency levels in requires_skill:", 
        paste(unique(invalid_rs), collapse = ", ") )
      )
    }
  }
  
  # Check teaches proficiency values
  if ("teaches" %in% names(lg$ edges)) {
    ts_df <- lg$ edges$ teaches
    if ("skill_level_taught" %in% names(ts_df)) {
      ts_levels <- ts_df$ skill_level_taught
    } else if ("proficiency" %in% names(ts_df)) {
      ts_levels <- ts_df$ proficiency
    } else {
      ts_levels <- integer()
      add_warning("teaches missing proficiency column")
    }
    invalid_ts <- ts_levels[ !ts_levels %in% valid_levels ]
    if ( length(invalid_ts) > 0 ) {
      add_error(paste(
        "Invalid proficiency levels in teaches:", 
        paste(unique(invalid_ts), collapse = ", ") ) 
      )
    }
  }
  
  # --- Referential integrity checks ---
  
  # Get valid IDs
  skill_ids <- if ( "skills" %in% names(lg$ nodes) ) {
    lg$ nodes$ skills$ skill_id
  } else {
    integer()
  }
  learner_ids <- if ( "learners" %in% names(lg$ nodes) ) {
    lg$ nodes$ learners$ learner_id
  } else {
    integer()
  }
  role_ids <- if ( "work_roles" %in% names(lg$ nodes) ) {
    lg$ nodes$ work_roles$ role_id
  } else {
    integer()
  }
  course_ids <- if ( "courses" %in% names(lg$ nodes) ) {
    lg$ nodes$ courses$ course_id
  } else {
    integer()
  }
  cmp_ids <- if ( "competencies" %in% names(lg$ nodes) ) {
    lg$ nodes$ competencies$ cmp_id
  } else {
    integer()
  }
  
  # Check has_skill references
  if ( "has_skill" %in% names(lg$ edges) ) {
    hs <- lg$ edges$ has_skill
    bad_learners <- 
      hs$ learner_id [ !hs$ learner_id %in% learner_ids ]
    bad_skills <- 
      hs$ skill_id [ !hs$ skill_id %in% skill_ids ]
    if ( length(bad_learners) > 0 ) {
      add_error(paste(
        "has_skill references invalid learner_ids:", 
        paste(unique(bad_learners), collapse = ", ") ) 
      )
    }
    if ( length(bad_skills) > 0 ) {
      add_error(paste(
        "has_skill references invalid skill_ids:", 
        paste(unique(bad_skills), collapse = ", ") ) 
      )
    }
  }
  
  # Check requires_skill references
  if ( "requires_skill" %in% names(lg$ edges) ) {
    rs <- lg$ edges$ requires_skill
    bad_roles  <- rs$ role_id [ !rs$ role_id %in% role_ids ]
    bad_skills <- rs$ skill_id [ !rs$ skill_id %in% skill_ids ]
    if ( length(bad_roles) > 0 ) {
      add_error(paste(
        "requires_skill references invalid role_ids:", 
        paste(unique(bad_roles), collapse = ", ") ) 
      )
    }
    if ( length(bad_skills) > 0 ) {
      add_error(paste(
        "requires_skill references invalid skill_ids:", 
        paste(unique(bad_skills), collapse = ", ") ) 
      )
    }
  }
  
  # Check prerequisite references
  if ( "prerequisite" %in% names(lg$ edges) ) {
    pr <- lg$ edges$ prerequisite
    bad_from <- pr$ skill_from_id [ !pr$skill_from_id %in% skill_ids ]
    bad_to   <- pr$ skill_to_id   [ !pr$skill_to_id   %in% skill_ids ]
    if ( length(bad_from) > 0 ) {
      add_error(paste(
        "prerequisite references invalid skill_from_ids:", 
        paste(unique(bad_from), collapse = ", ") ) 
      )
    }
    if ( length(bad_to) > 0 ) {
      add_error(paste(
        "prerequisite references invalid skill_to_ids:", 
        paste(unique(bad_to), collapse = ", ") ) 
      )
    }
  }
  
  # Check teaches references
  if ( "teaches" %in% names(lg$ edges) ) {
    tc <- lg$ edges$ teaches
    bad_courses <- tc$ course_id [ !tc$ course_id %in% course_ids ]
    bad_skills  <- tc$ skill_id  [ !tc$ skill_id  %in% skill_ids ]
    if ( length(bad_courses) > 0 ) {
      add_error(paste(
        "teaches references invalid course_ids:", 
        paste(unique(bad_courses), collapse = ", ") ) 
      )
    }
    if ( length(bad_skills) > 0 ) {
      add_error(paste(
        "teaches references invalid skill_ids:", 
        paste(unique(bad_skills), collapse = ", ") ) 
      )
    }
  }
  
  # Check skill_in_competency references
  # TODO: confirm or change name of target ID for skill_in_competency:
  #       current:  competency_id
  #       proposed: cmp_id
  if ( "skill_in_competency" %in% names(lg$ edges) ) {
    sic <- lg$ edges$ skill_in_competency
    bad_skills <- sic$ skill_id [ !sic$ skill_id %in% skill_ids ]
    bad_cmps   <- sic$ competency_id [ !sic$ competency_id %in% cmp_ids ]
    if ( length(bad_skills) > 0 ) {
      add_error(paste(
        "skill_in_competency references invalid skill_ids:", 
        paste(unique(bad_skills), collapse = ", ") ) 
      )
    }
    if ( length(bad_cmps) > 0 ) {
      add_error(paste(
        "skill_in_competency references invalid competency_ids:", 
        paste(unique(bad_cmps), collapse = ", ") ) 
      )
    }
  }
  
  # --- Report results ---
  
  is_valid <- length(errors) == 0
  
  if (verbose) {
    cat("LearningGraph Schema Validation\n")
    cat("================================\n\n")
    
    if (is_valid) {
      cat("Status: VALID\n\n")
    } else {
      cat("Status: INVALID\n\n")
    }
    
    if ( length(errors) > 0 ) {
      cat("Errors:\n")
      for (e in errors) {
        cat("  [ERROR] ", e, "\n")
      }
      cat("\n")
    }
    
    if ( length(warnings) > 0 ) {
      cat("Warnings:\n")
      for (w in warnings) {
        cat("  [WARN]  ", w, "\n")
      }
      cat("\n")
    }
    
    if (is_valid && length(warnings) == 0) {
      cat("No errors or warnings.\n")
    }
  }
  
  invisible(list(
    valid    = is_valid,
    errors   = errors,
    warnings = warnings
  ))
}


##
#   lg_print_schema()
##
#' Print the LearningGraph Schema
#'
#' Displays a formatted summary of the schema for documentation purposes.
#'
#' @param schema Optional schema object; uses lg_schema() if not provided
#'
#' @return Invisibly returns the schema
#' @export
#'
#' @examples
#' lg_print_schema()
lg_print_schema <- function(schema = NULL) {
  
  if (is.null(schema)) {
    schema <- lg_schema()
  }
  
  cat("LearningGraph Schema (", schema$ metadata$ version, ")\n", sep = "")
  cat(strrep("=", 50), "\n\n")
  
  cat(schema$ metadata$ description, "\n\n")
  
  # Node types
  cat("NODE TYPES\n")
  cat(strrep("-", 50), "\n")
  for (i in seq_len(nrow(schema$ node_types))) {
    nt <- schema$ node_types [i, ]
    cat(sprintf("  %-12s %s\n", nt$ type, nt$ description))
  }
  cat("\n")
  
  # Edge types
  cat("EDGE TYPES\n")
  cat(strrep("-", 50), "\n")
  for (i in seq_len(nrow(schema$ edge_types))) {
    et <- schema$ edge_types [i, ]
    prop_str <- if ( is.na(et$ property) ) {
      ""
    } else {
      paste0(" [", et$ property, "]")
    }
    cat(sprintf(
      "  %s: %s -> %s%s\n", 
      et$ edge_type, 
      et$ source_type, 
      et$ target_type, 
      prop_str) )
    cat(sprintf(
      "      %s\n", 
      et$ description ) )
  }
  cat("\n")
  
  # Proficiency levels
  cat("PROFICIENCY LEVELS\n")
  cat(strrep("-", 50), "\n")
  for (i in seq_len(nrow(schema$ proficiency_levels))) {
    pl <- schema$ proficiency_levels [i, ]
    cat(sprintf(
      "  %d - %-12s %s\n", 
      pl$ level, 
      pl$ label, 
      pl$ description ) )
  }
  cat("\n")
  
  # Temporal invariants
  cat("TEMPORAL INVARIANTS (documented, not enforced)\n")
  cat(strrep("-", 50), "\n")
  for (i in seq_len(nrow(schema$ temporal_invariants))) {
    ti <- schema$ temporal_invariants [i, ]
    cat(sprintf(
      "  %s\n      %s\n", 
      ti$ invariant, 
      ti$ description ) )
  }
  
  invisible(schema)
}


##
#   lg_schema_to_markdown()
##
#' Generate Schema Documentation as Markdown
#'
#' Creates a markdown-formatted documentation string suitable for
#' inclusion in Quarto documents or README files.
#'
#' @param schema Optional schema object; uses lg_schema() if not provided
#'
#' @return A character string containing markdown-formatted documentation
#' @export
#'
#' @examples
#' md <- lg_schema_to_markdown()
#' cat(md)
lg_schema_to_markdown <- function(schema = NULL) {
  
  if (is.null(schema)) {
    schema <- lg_schema()
  }
  
  lines <- character()
  add <- function(...) {
    lines <<- c(lines, paste0(...))
  }
  
  add("## LearningGraph Schema (v", schema$metadata$version, ")\n")
  add("")
  add(schema$metadata$description, "\n")
  add("")
  
  # Edge types table
  add("### Edge Types\n")
  add("")
  add("| Edge Type | Source | Target | Property | Semantics |")
  add("|-----------|--------|--------|----------|-----------|")
  
  for (i in seq_len(nrow(schema$edge_types))) {
    et <- schema$edge_types[i, ]
    prop <- if (is.na(et$property)) "—" else et$property
    sem <- if (is.na(et$property_semantics)) "—" else et$property_semantics
    add("| `", et$edge_type, "` | ", et$source_type, " | ", 
        et$target_type, " | ", prop, " | ", sem, " |")
  }
  add("")
  
  # Proficiency levels table
  add("### Proficiency Levels\n")
  add("")
  add("| Level | Label | Description |")
  add("|-------|-------|-------------|")
  
  for (i in seq_len(nrow(schema$proficiency_levels))) {
    pl <- schema$proficiency_levels[i, ]
    add("| ", pl$level, " | ", pl$label, " | ", pl$description, " |")
  }
  add("")
  
  return(
    paste(lines, collapse = "\n") )
  
}


##
#  EOF
##
