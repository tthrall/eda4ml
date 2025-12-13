#####
###
#     graph_helpers.R
#
#       Functions supporting eda4ml chapter on graph theory.
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
#  gen_learning_lst()
##
gen_learning_lst <- function() {
  learners <- tibble::tribble(
    ~nm_pref, ~sex, ~role, ~org, 
    "Alice", "female", "student", "XU", 
    "Beth", "female", "employee", "EVC", 
    "Charlie", "male", "student", "XU", 
    "Dan", "male", "student", "XU", 
    "Elliot", "male", "employee", "EVC", 
    "Fiona", "female", "employee", "EVC"
  ) |> 
    dplyr::mutate(across(
      .cols = c(sex, role), 
      .fns = forcats::as_factor
    ))
  
  courses <- tibble::tribble(
    ~p_type, ~p_nm, ~inst, ~course_nm, 
    "cert", "Agentic AI", "JHU", "Foundations of Agentic AI", 
    "cert", "Agentic AI", "JHU", "Core Agent Capabilities", 
    "cert", "Agentic AI", "JHU", "Symbolic, BDI & LLM-Based Architectures", 
    "cert", "Agentic AI", "JHU", "Prompt Engineering Techniques", 
    "cert", "Agentic AI", "JHU", "Reinforcement Learning in Agents", 
    "cert", "Agentic AI", "JHU", "Multi-Agent Systems", 
    "cert", "Agentic AI", "JHU", "Human-Agent Collaboration", 
    "cert", "Agentic AI", "JHU", "Agentic AI Ethics, Safety & Alignment", 
    "cert", "Agentic AI", "JHU", "Agent Architecture", 
    "cert", "Agentic AI", "JHU", "Symbolic Reasoning", 
    "MA", "Data Science", "JHU", "Algorithms for Data Science", 
    "MA", "Data Science", "JHU", "Statistical Methods and Data Analysis", 
    "MA", "Data Science", "JHU", "Data Engineering Principles and Practice", 
    "MA", "Data Science", "JHU", "Data Patterns and Representations"
  )
  
  work_roles <- tibble::tribble(
    ~tag, ~nm, 
    "DA", "Data Analyst", 
    "DSci", "Data Scientist", 
    "DStew", "Data Steward", 
    "AI_ML", "AI / ML Specialist", 
    "DOps", "Data Operations Specialist", 
    "DArch", "Data Architect", 
    "AI_Test", "AI Test & Evaluation Specialist", 
    "AI_Risk", "AI Risk & Ethics Specialist", 
    "AI_Adopt", "AI Adoption Specialist", 
    "AI_Inn_Lead", "AI Innovation Leader", 
    "DOfcr", "Data Officer"
  )
  
  # read these LG entities from TSV files
  skills       <- get_ksa_2022()
  competencies <- get_cmp_2022()
  
  # LG relationships
  relations <- tibble::tibble(
    relation = c(
      "has_skill", 
      "requires_skill", 
      "teaches", 
      "prerequisite", 
      "covers", 
      "time_to_acquire"
    ), 
    ent_type_1 = c(
      "learner", 
      "work_role", 
      "course", 
      "skill", 
      "course", 
      "learner"
    ), 
    ent_type_2 = c(
      "skill", 
      "skill", 
      "skill", 
      "skill", 
      "competency", 
      "skill"
    ), 
    comment = c(
      "current_state", 
      "goal_state", 
      "path_learning_edge", 
      "skill_ordering", 
      "clustering", 
      "path_optimal"
    )
  )
  
  # Learning Graph entities and relationships
  LG_lst <- list(
    learners     = learners, 
    skills       = skills, 
    courses      = courses, 
    work_roles   = work_roles, 
    competencies = competencies, 
    relations    = relations
  )
  
  return(LG_lst)
}

##
#  get_ksa_2022()
#  
#    tibble of DSci Knowledge, Skills, and Abilities (KSAs)
#  
#    source: 
#      Competency Resource Guide for Data Science
#      Office of the Director of National Intelligence
#      2023-04-28
##
get_ksa_2022 <- function() {
  ksa_2022 <- readr::read_tsv(here::here(
    "data", "retain", "ksa_2022.txt"
  ))
}

##
#  get_cmp_2022()
#  
#    tibble of DSci competencies
#  
#    source: 
#      Competency Resource Guide for Data Science
#      Office of the Director of National Intelligence
#      2023-04-28
##
get_cmp_2022 <- function() {
  cmp_2022 <- readr::read_tsv(here::here(
    "data", "retain", "cmp_2022.txt"
  ))
}


##
#  EOF
##
