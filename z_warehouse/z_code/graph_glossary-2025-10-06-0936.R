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
  graph_glossary <- tibble::tribble(
    ~term, ~dscr, 
    "acyclic", "a graph is acyclic if it has no cycles", 
    "adjacency", "binary indicator: given vertices are / are not endpoints of a common edge", 
    "connected component", "a maximal connected subgraph", 
    "connected graph", "a graph is connected if each pair of vertices is connected", 
    "connected vertices", "a pair of vertices that co-occur in some path", 
    "cycle", "a finite path whose first and last vertex are the same", 
    "degree", "number of incident edges of a vertex", 
    "directed edge", "an ordered pair of vertices (called endpoints of the edge)", 
    "edge", "a specified pair of vertices (in a hypergraph, more than two vertices)", 
    "edge weight", "a numerical value assigned to an edge", 
    "graph", "a system of vertices (nodes) and edges", 
    "incidence", "if a vertex is an endpoint of an edge, the (vertex, edge) pair is said to be incident", 
    "neighborhood, 1-hop", "the subgraph of vertices adjacent to a referenced vertex", 
    "neighborhood, 1.5-hop", "the subgraph induced by a vertex, its adjacent vertices, and their adjacent vertices", 
    "neighborhood, 2-hop", "the subgraph of vertices of distance 2 from a referenced vertex", 
    "path", "a sequence of adjacent vertices", 
    "simple path", "a path in which no vertex is repeated", 
    "subgraph", "a subset of edges along with their endpoints and possibly additional vertices", 
    "subgraph, induced", "a subset of vertices along with the edges having both endpoints in the subset", 
    "tree, directed", "a directed graph having a distinguished root vertex R such that there is exactly one path from R to any other vertex V", 
    "tree, undirected", "a connected, acyclic graph"
  )
  return(graph_glossary)
}


##
#  EOF
##
