#####
###
#     graph_glossary.R
#
#       Record the handedness data reported in FPP (4e).
#
#       Statistics (4e) by Freedman, Pisani, and Purves.
#       Chapter 28: The Chi-Squared Test
#       Section  4: Testing Independence
###
#####

##
#  gen_graph_glossary()
##
gen_graph_glossary <- function() {
  term_vec <- c(
    "acyclic", 
    "adjacency", 
    "connected component", 
    "connected graph", 
    "connected vertices", 
    "cycle", 
    "degree", 
    "directed edge", 
    "edge", 
    "edge weight", 
    "graph", 
    "incidence", 
    "neighborhood, 1-hop", 
    "neighborhood, 1.5-hop", 
    "neighborhood, 2-hop", 
    "path", 
    "simple path", 
    "subgraph", 
    "subgraph, induced", 
    "tree, directed", 
    "tree, undirected"
  )
  
  dscr_vec <- c(
    "a graph is acyclic if it has no cycles", 
    "binary indicator: given vertices are / are not endpoints of a common edge", 
    "a maximal connected subgraph", 
    "a graph is connected if each pair of vertices is connected", 
    "a pair of vertices that co-occur in some path", 
    "a finite path whose first and last vertex are the same", 
    "number of incident edges of a vertex", 
    "an ordered pair of vertices (called endpoints of the edge)", 
    "a specified pair of vertices (in a hypergraph, more than two vertices)", 
    "a numerical value assigned to an edge", 
    "a system of vertices (nodes) and edges", 
    "if a vertex is an endpoint of an edge, the (vertex, edge) pair is said to be incident", 
    "the subgraph of vertices adjacent to a referenced vertex", 
    "the subgraph induced by a vertex, its adjacent vertices, and their adjacent vertices", 
    "the subgraph of vertices of distance 2 from a referenced vertex", 
    "a sequence of adjacent vertices", 
    "a path in which no vertex is repeated", 
    "a subset of edges along with their endpoints and possibly additional vertices", 
    "a subset of vertices along with the edges having both endpoints in the subset", 
    "a directed graph having a distinguished root vertex R such that there is exactly one path from R to any other vertex V", 
    "a connected, acyclic graph"
  )
  
  graph_glossary <- tibble::tibble(
    term = term_vec, 
    dscr = dscr_vec)
  return(graph_glossary)
}


##
#  EOF
##
