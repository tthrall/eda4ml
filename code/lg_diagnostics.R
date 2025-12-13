#####
###
#     lg_diagnostics.R
#
#       Diagnostic functions for analyzing LearningGraph connectivity
#       and ensuring completeness of learning paths.
###
#####

##
#  lg_diagnose_connectivity()
##
#' Diagnose connectivity issues in the LearningGraph prerequisite DAG
#'
#' @param lg A LearningGraph object
#' @return A list with diagnostic information
#' @export
lg_diagnose_connectivity <- function(lg) {
  
  g <- lg_to_igraph(lg, "prerequisite")
  skills <- lg$nodes$skills
  
  # 1. Find source nodes (no prerequisites - entry points for learning)
  in_degrees <- igraph::degree(g, mode = "in")
  source_nodes <- names(in_degrees[in_degrees == 0])
  
  # 2. Find sink nodes (nothing depends on them - terminal skills)
  out_degrees <- igraph::degree(g, mode = "out")
  sink_nodes <- names(out_degrees[out_degrees == 0])
  
  # 3. Find isolated nodes (not connected to anything)
  total_degrees <- igraph::degree(g, mode = "all")
  isolated_nodes <- names(total_degrees[total_degrees == 0])
  
  # 4. Check which skills are reachable from each source
  reachability <- list()
  for (src in source_nodes) {
    # Get all nodes reachable from this source
    reachable <- names(igraph::subcomponent(g, src, mode = "out"))
    reachability[[src]] <- reachable
  }
  
  # 5. Find skills NOT reachable from ANY source
  all_skills <- igraph::V(g)$name
  reachable_from_any_source <- unique(unlist(reachability))
  unreachable_skills <- setdiff(all_skills, reachable_from_any_source)
  
  # 6. Check connectivity of underlying undirected graph
  g_undirected <- igraph::as_undirected(g)
  components <- igraph::components(g_undirected)
  n_components <- components$no
  
  # 7. For each sink, find which sources can reach it
  sink_reachability <- list()
  for (sink in sink_nodes) {
    sources_reaching <- c()
    for (src in source_nodes) {
      if (sink %in% reachability[[src]]) {
        sources_reaching <- c(sources_reaching, src)
      }
    }
    sink_reachability[[sink]] <- sources_reaching
  }
  
  # 8. Find all unreachable pairs (source → sink with no path)
  unreachable_pairs <- tibble::tibble(
    from = character(),
    to = character()
  )
  
  for (src in source_nodes) {
    for (sink in sink_nodes) {
      if (!(sink %in% reachability[[src]])) {
        unreachable_pairs <- dplyr::bind_rows(
          unreachable_pairs,
          tibble::tibble(from = src, to = sink)
        )
      }
    }
  }
  
  # Compile diagnostics
  diagnostics <- list(
    # Basic stats
    n_skills = length(all_skills),
    n_edges = igraph::ecount(g),
    n_components = n_components,
    is_dag = igraph::is_dag(g),
    
    # Node classification
    source_nodes = source_nodes,
    sink_nodes = sink_nodes,
    isolated_nodes = isolated_nodes,
    
    # Reachability
    reachability_from_sources = reachability,
    unreachable_skills = unreachable_skills,
    sink_reachability = sink_reachability,
    unreachable_pairs = unreachable_pairs,
    
    # Summary flags
    all_skills_reachable = length(unreachable_skills) == 0,
    fully_connected = n_components == 1,
    has_isolated_nodes = length(isolated_nodes) > 0
  )
  
  return(diagnostics)
}


##
#  lg_print_diagnostics()
##
#' Print a readable summary of LearningGraph diagnostics
#'
#' @param dg Output from lg_diagnose_connectivity()
#' @export
lg_print_diagnostics <- function(dg) {
  
  cat("LearningGraph Connectivity Diagnostics\n")
  cat("======================================\n\n")
  
  cat("Basic Structure:\n")
  cat("  Skills:     ", dg$n_skills, "\n")
  cat("  Edges:      ", dg$n_edges, "\n")
  cat("  Components: ", dg$n_components, "\n")
  cat("  Is DAG:     ", dg$is_dag, "\n")
  
  cat("\nNode Classification:\n")
  cat("  Entry points (no prerequisites):\n")
  cat("    ", paste(dg$source_nodes, collapse = ", "), "\n")
  cat("  Terminal skills (nothing depends on them):\n")
  cat("    ", paste(dg$sink_nodes, collapse = ", "), "\n")
  
  if (dg$has_isolated_nodes) {
    cat("\n  WARNING - Isolated nodes (disconnected):\n")
    cat("    ", paste(dg$isolated_nodes, collapse = ", "), "\n")
  }
  
  cat("\nReachability Analysis:\n")
  if (dg$all_skills_reachable) {
    cat("  OK: All skills reachable from at least one entry point\n")
  } else {
    cat("  WARNING - Skills not reachable from any entry point:\n")
    cat("    ", paste(dg$unreachable_skills, collapse = ", "), "\n")
  }
  
  if (dg$fully_connected) {
    cat("  OK: Graph is fully connected (single component)\n")
  } else {
    cat("  WARNING: Graph has ", dg$n_components, " disconnected components\n")
  }
  
  cat("\nSource → Sink Reachability:\n")
  for (sink in names(dg$sink_reachability)) {
    sources <- dg$sink_reachability[[sink]]
    if (length(sources) == 0) {
      cat("  WARNING: ", sink, " not reachable from any entry point!\n")
    } else if (length(sources) == length(dg$source_nodes)) {
      cat("  ", sink, ": reachable from ALL entry points\n")
    } else {
      cat("  ", sink, ": reachable from ", paste(sources, collapse = ", "), "\n")
    }
  }
  
  if (nrow(dg$unreachable_pairs) > 0) {
    cat("\nUnreachable Pairs (may need bridging edges):\n")
    for (i in seq_len(nrow(dg$unreachable_pairs))) {
      cat("  ", dg$unreachable_pairs$from[i], " → ", 
          dg$unreachable_pairs$to[i], "\n")
    }
  } else {
    cat("\nOK: All source → sink pairs have valid paths\n")
  }
  
  invisible(dg)
}


##
#  lg_suggest_bridges()
##
#' Suggest bridging edges to improve connectivity
#'
#' @param lg A LearningGraph object
#' @param dg Output from lg_diagnose_connectivity()
#' @return A tibble of suggested edges
#' @export
lg_suggest_bridges <- function(lg, dg) {
  
  suggestions <- tibble::tibble(
    from_skill = character(),
    to_skill = character(),
    reason = character()
  )
  
  # For each unreachable pair, suggest a bridge
  if (nrow(dg$unreachable_pairs) > 0) {
    g <- lg_to_igraph(lg, "prerequisite")
    
    for (i in seq_len(nrow(dg$unreachable_pairs))) {
      src <- dg$unreachable_pairs$from[i]
      sink <- dg$unreachable_pairs$to[i]
      
      # Find what src can reach
      src_reaches <- names(igraph::subcomponent(g, src, mode = "out"))
      
      # Find what can reach sink
      sink_reached_by <- names(igraph::subcomponent(g, sink, mode = "in"))
      
      # The bridge should connect something src reaches to something that reaches sink
      # Simplest: direct edge from a src-reachable node to sink
      # Best candidate: the "most advanced" skill reachable from src
      
      # For now, suggest the most downstream skill from src → sink
      out_degrees <- igraph::degree(g, v = src_reaches, mode = "out")
      # Skills with low out-degree are more "terminal" in the src's subtree
      candidates <- names(sort(out_degrees))
      
      if (length(candidates) > 0) {
        best_bridge_from <- candidates[1]  # Most terminal in src's reach
        
        suggestions <- dplyr::bind_rows(
          suggestions,
          tibble::tibble(
            from_skill = best_bridge_from,
            to_skill = sink,
            reason = paste0("Connects ", src, " path to ", sink)
          )
        )
      }
    }
  }
  
  # Handle isolated nodes
  if (length(dg$isolated_nodes) > 0) {
    for (node in dg$isolated_nodes) {
      suggestions <- dplyr::bind_rows(
        suggestions,
        tibble::tibble(
          from_skill = "(entry point)",
          to_skill = node,
          reason = "Node is completely isolated"
        )
      )
    }
  }
  
  # Remove duplicates
  suggestions <- dplyr::distinct(suggestions)
  
  return(suggestions)
}


##
#  run_lg_diagnostics()
##
#' Run full diagnostics on the LearningGraph
#'
#' @return The diagnostic results (invisibly)
#' @export
run_lg_diagnostics <- function() {
  lg <- build_learning_graph()
  dg <- lg_diagnose_connectivity(lg)
  lg_print_diagnostics(dg)
  
  cat("\n")
  suggestions <- lg_suggest_bridges(lg, dg)
  
  if (nrow(suggestions) > 0) {
    cat("Suggested Bridging Edges:\n")
    cat("=========================\n")
    print(suggestions, n = Inf)
  } else {
    cat("No bridging edges needed - graph is well-connected!\n")
  }
  
  invisible(dg)
}

##
#  EOF
##
