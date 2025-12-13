# lg_schema_examples.R
# Demonstration of LearningGraph schema functions
#
# Run after sourcing lg_schema.R and learning_graph.R

# --- Example 1: View the schema ---

cat("=== Example 1: Print Schema ===\n\n")
lg_print_schema()

# --- Example 2: Validate an existing LearningGraph ---

cat("\n=== Example 2: Validate LearningGraph ===\n\n")

# Assuming you have create_learning_graph() from learning_graph.R
# lg <- create_learning_graph()
# lg_validate(lg)

# Or if you have tst_lg from run_lg_examples():
# lg_validate(tst_lg)

# --- Example 3: Programmatic access to schema ---

cat("\n=== Example 3: Programmatic Schema Access ===\n\n")

schema <- lg_schema()

# Which edge types carry proficiency?
edges_with_prof <- schema$edge_types |>
  dplyr::filter(!is.na(property)) |>
  dplyr::select(edge_type, property_semantics)

cat("Edge types with proficiency property:\n")
print(edges_with_prof)

# What are the entry-level proficiency descriptors?
cat("\nProficiency level guidance:\n")
schema$proficiency_levels |>
  dplyr::select(level, label, guidance_needed) |>
  print()

# --- Example 4: Generate Markdown for documentation ---

cat("\n=== Example 4: Markdown Export ===\n\n")

md_doc <- lg_schema_to_markdown()
cat("First 500 characters of markdown output:\n")
cat(substr(md_doc, 1, 500), "...\n")

# To write to file:
# writeLines(md_doc, "lg_schema_doc.md")

# --- Example 5: Schema-aware gap analysis ---

cat("\n=== Example 5: Schema Metadata in Analysis ===\n\n")

# The schema can inform analysis functions
# For example, interpreting proficiency gaps:

interpret_gap <- function(gap, schema = NULL) {
  if (is.null(schema)) schema <- lg_schema()
  
  levels <- schema$proficiency_levels
  
  case_when <- function(...) dplyr::case_when(...)
  
  interpretation <- case_when(
    gap == 0 ~ "Requirement met",
    gap == 1 ~ "Minor gap (targeted practice)",
    gap == 2 ~ "Moderate gap (focused training)",
    gap >= 3 ~ "Significant gap (structured learning path)"
  )
  
  interpretation
}

# Example gaps from Alice's skill gap analysis
example_gaps <- c(3, 3, 2, 1, 0)
cat("Gap interpretations:\n")
for (g in example_gaps) {
  cat(sprintf("  Gap of %d: %s\n", g, interpret_gap(g)))
}
