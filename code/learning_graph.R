#####
###
#     learning_graph.R
#
#       LearningGraph: A knowledge graph dataset for demonstrating
#       graph theory concepts in the context of skills-based learning.
#
#       Based on the IC Data Science Competency Resource Guide (CRG) 2023
#       and inspired by Workera.ai's skills intelligence platform.
#
#       For inclusion in eda4mldata package.
#
#     Version History:
#       1.0.0 - Initial version
#       1.0.1 - Added feature_engineering → statistical_learning bridge
#       1.0.2 - Improved connectivity: fixed Data Collection isolation,
#               added cross-track bridges for EDA→Experimental Design
#               and Optimization→Statistical Learning
###
#####

## 
#  build_learning_graph()
## 
#' Build the LearningGraph dataset
#'
#' @return A list containing nodes (by type) and edges (by relation type)
#' @export
build_learning_graph <- function() {

  # =========================================================================
  # PROFICIENCY LEVELS (from CRG Appendix A)
  # =========================================================================
  proficiency_levels <- tibble::tribble(
    ~level, ~label,        ~guidance_needed,            ~development_focus,
    0L,     "None",        "N/A",                       "N/A",
    1L,     "Basic",       "Frequently",                "Learning established methods for routine situations",
    2L,     "Intermediate","Occasionally",              "Depth to address difficult, novel situations",
    3L,     "Advanced",    "Rarely",                    "Blending skills for complex, ambiguous situations",
    4L,     "Master",      "None (recognized authority)","Continuing education for breadth and currency"
  )

  # =========================================================================
  # NODE TYPE 1: COMPETENCIES (all 7 from CRG)
  # =========================================================================
  competencies <- tibble::tribble(
    ~cmp_id, ~cmp_tag,       ~cmp_name,                                      ~cmp_abbrev,
    1L,      "compute_fndns","Computational Foundations of Data Science",    "Computation",
    2L,      "collab",       "Cross-Disciplinary Data Science Collaboration","Collaboration",
    3L,      "data_engr",    "Data Engineering for Data Science",            "Data Engineering",
    4L,      "ds_ai_gov",    "Data Science/AI Governance and Ethics",        "Governance",
    5L,      "math_fndns",   "Mathematical Foundations of Data Science",     "Math",
    6L,      "data_analysis","Scientific Data Analysis",                     "Analysis",
    7L,      "stats_fndns",  "Statistical Foundations of Data Science",      "Statistics"
  )

  # =========================================================================
  # NODE TYPE 2: SKILLS (KSAs) - Curated subset for EDA4ML scope
  #
  # Selection criteria:
  #   - Focus on competencies 5, 6, 7 (Math, Analysis, Statistics)
  #   - Include key computational skills (programming, algorithms)
  #   - Include collaboration skills relevant to data narratives
  #   - ~18 skills for manageable visualization
  # =========================================================================
  skills <- tibble::tribble(
    ~skill_id, ~skill_tag,          ~skill_name,              ~cmp_id, ~k_or_s, ~description,
    # Computational Foundations (cmp_id = 1) - select 2
    1L,  "algorithms",        "Algorithms",                  1L, "k", "Knowledge of designing and implementing algorithms, from algorithmic thinking through ML methods",
    2L,  "programming",       "Programming",                 1L, "s", "Skill in programming in compiled and interpreted languages with software development practices",

    # Collaboration (cmp_id = 2) - select 3
    3L,  "problem_formulation","Problem Formulation",        2L, "s", "Skill in approximating domain problems with data science questions",
    4L,  "limits",            "Limitations",                 2L, "s", "Skill in communicating the limitations of data and models",
    5L,  "data_narratives",   "Data Narratives",             2L, "s", "Skill in building data narratives that communicate principled inferences",

    # Data Engineering (cmp_id = 3) - select 1
    6L,  "data_collection",   "Data Collection",             3L, "s", "Skill in gathering structured or unstructured datasets",

    # Math Foundations (cmp_id = 5) - select 4
    7L,  "probability_theory","Probability Theory",          5L, "k", "Knowledge of probability theory, from independence to stochastic processes",
    8L,  "linear_algebra",    "Linear Algebra",              5L, "k", "Knowledge of vectors, matrices, abstract vector spaces, and numerical methods",
    9L,  "graph_theory",      "Graph Theory",                5L, "k", "Knowledge of nodes, edges, and algorithmic solutions like shortest path",
    10L, "optimization",      "Optimization",                5L, "k", "Knowledge of mathematical optimization, from calculus to constrained problems",

    # Scientific Data Analysis (cmp_id = 6) - select 4
    11L, "data_cleaning",     "Data Cleaning",               6L, "s", "Skill in preparing data by handling missing or low-quality records",
    12L, "EDA",               "Exploratory Data Analysis",   6L, "s", "Skill in iterative visualization, summarization, and unsupervised learning",
    13L, "data_visualization","Data Visualization",          6L, "s", "Skill in displaying data to enable comparisons and enhance comprehension",
    14L, "feature_engineering","Feature Engineering",        6L, "s", "Skill in transforming data guided by domain knowledge",

    # Statistical Foundations (cmp_id = 7) - select 4
    15L, "experimental_design","Experimental Design",        7L, "k", "Knowledge of designing surveys, experiments, and observational studies",
    16L, "linear_models",     "Linear Models",               7L, "k", "Knowledge from simple linear models through generalized linear models",
    17L, "inference_prediction","Inference and Prediction",  7L, "k", "Knowledge of estimation, predictive inference, hypothesis testing, simulation",
    18L, "statistical_learning","Statistical Learning",      7L, "k", "Knowledge of algorithms from nearest-neighbor to neural nets"
  )

  # =========================================================================
  # NODE TYPE 3: WORK ROLES - Curated subset (3 roles)
  # =========================================================================
  work_roles <- tibble::tribble(
    ~role_id, ~role_tag, ~role_name,          ~role_description,
    1L, "DA",     "Data Analyst",       "Analyzes data and builds visualizations to report insights",
    2L, "DSci",   "Data Scientist",     "Combines scientific method, math, programming, and storytelling",
    3L, "AI_ML",  "AI/ML Specialist",   "Designs and develops AI applications and solutions"
  )

  # =========================================================================
  # NODE TYPE 4: COURSES - Curated to align with EDA4ML chapters
  # =========================================================================
  courses <- tibble::tribble(
    ~course_id, ~course_tag,    ~course_name,                            ~provider,
    1L, "stat_methods",  "Statistical Methods and Data Analysis",  "JHU",
    2L, "algo_ds",       "Algorithms for Data Science",            "JHU",
    3L, "data_patterns", "Data Patterns and Representations",      "JHU",
    4L, "data_engr",     "Data Engineering Principles",            "JHU",
    5L, "agent_fndns",   "Foundations of Agentic AI",              "JHU",
    6L, "ml_fndns",      "Machine Learning Foundations",           "Coursera"
  )

  # =========================================================================
  # NODE TYPE 5: LEARNERS - Fictional profiles with varying backgrounds
  # =========================================================================
  learners <- tibble::tribble(
    ~learner_id, ~name,    ~role,       ~organization, ~background,
    1L, "Alice",   "student",   "Xavier U",   "Math major, junior year",
    2L, "Beth",    "employee",  "DataCorp",   "Senior analyst, 5 years experience",
    3L, "Charlie", "student",   "Xavier U",   "CS major, senior year",
    4L, "Dan",     "student",   "Xavier U",   "Statistics minor, interested in ML",
    5L, "Elliot",  "employee",  "DataCorp",   "Junior data engineer, 1 year",
    6L, "Fiona",   "employee",  "DataCorp",   "Mid-level data scientist, 3 years"
  )

  # =========================================================================
  # EDGE TYPE 1: has_skill (Learner → Skill)
  #   Edge weight = current proficiency level (0-4)
  #
  # Design notes:
  #   - Alice: Strong math, weak programming/applied
  #   - Beth: Strong applied/visualization, moderate theory
  #   - Charlie: Strong programming, weak statistics
  #   - Dan: Balanced but all at basic/intermediate level
  #   - Elliot: Data engineering focus, weak on theory
  #   - Fiona: Well-rounded data scientist profile
  # =========================================================================
  has_skill <- tibble::tribble(
    ~learner_id, ~skill_id, ~proficiency,
    # Alice - Math major (strong theory, weak applied)
    1L,  7L,  3L,   # probability_theory - Advanced
    1L,  8L,  3L,   # linear_algebra - Advanced
    1L, 10L,  2L,   # optimization - Intermediate
    1L,  2L,  1L,   # programming - Basic
    1L, 12L,  1L,   # EDA - Basic
    1L, 16L,  2L,   # linear_models - Intermediate

    # Beth - Senior analyst (strong applied, moderate theory)
    2L,  2L,  3L,   # programming - Advanced
    2L, 11L,  4L,   # data_cleaning - Master
    2L, 12L,  4L,   # EDA - Master
    2L, 13L,  4L,   # data_visualization - Master
    2L,  5L,  3L,   # data_narratives - Advanced
    2L,  8L,  2L,   # linear_algebra - Intermediate
    2L, 16L,  3L,   # linear_models - Advanced
    2L, 15L,  2L,   # experimental_design - Intermediate

    # Charlie - CS major (strong programming, weak statistics)
    3L,  1L,  3L,   # algorithms - Advanced
    3L,  2L,  4L,   # programming - Master
    3L,  9L,  2L,   # graph_theory - Intermediate
    3L,  8L,  2L,   # linear_algebra - Intermediate
    3L, 12L,  1L,   # EDA - Basic
    3L, 18L,  2L,   # statistical_learning - Intermediate (knows ML, not stats foundation)

    # Dan - Statistics minor (balanced, moderate levels)
    4L,  7L,  2L,   # probability_theory - Intermediate
    4L,  8L,  2L,   # linear_algebra - Intermediate
    4L, 12L,  2L,   # EDA - Intermediate
    4L, 16L,  2L,   # linear_models - Intermediate
    4L, 17L,  2L,   # inference_prediction - Intermediate
    4L,  2L,  1L,   # programming - Basic

    # Elliot - Junior data engineer (data handling focus)
    5L,  2L,  2L,   # programming - Intermediate
    5L,  6L,  3L,   # data_collection - Advanced
    5L, 11L,  3L,   # data_cleaning - Advanced
    5L, 14L,  2L,   # feature_engineering - Intermediate
    5L, 12L,  1L,   # EDA - Basic

    # Fiona - Mid-level data scientist (well-rounded)
    6L,  2L,  3L,   # programming - Advanced
    6L,  7L,  3L,   # probability_theory - Advanced
    6L,  8L,  3L,   # linear_algebra - Advanced
    6L, 12L,  3L,   # EDA - Advanced
    6L, 13L,  3L,   # data_visualization - Advanced
    6L, 16L,  3L,   # linear_models - Advanced
    6L, 17L,  3L,   # inference_prediction - Advanced
    6L, 18L,  3L,   # statistical_learning - Advanced
    6L,  3L,  2L,   # problem_formulation - Intermediate
    6L,  5L,  2L    # data_narratives - Intermediate
  )

  # =========================================================================
  # EDGE TYPE 2: requires_skill (Work Role → Skill)
  #   Edge weight = minimum required proficiency level
  #
  # Design notes:
  #   - DA: Heavy on visualization/EDA, moderate on theory
  #   - DSci: Balanced across all areas, higher requirements
  #   - AI_ML: Heavy on algorithms/statistical_learning, programming
  # =========================================================================
  requires_skill <- tibble::tribble(
    ~role_id, ~skill_id, ~required_proficiency,
    # Data Analyst
    1L, 11L, 3L,   # data_cleaning - Advanced
    1L, 12L, 3L,   # EDA - Advanced
    1L, 13L, 4L,   # data_visualization - Master
    1L,  5L, 3L,   # data_narratives - Advanced
    1L,  2L, 2L,   # programming - Intermediate
    1L, 16L, 2L,   # linear_models - Intermediate
    1L,  4L, 2L,   # limits - Intermediate

    # Data Scientist
    2L,  2L, 3L,   # programming - Advanced
    2L,  7L, 3L,   # probability_theory - Advanced
    2L,  8L, 3L,   # linear_algebra - Advanced
    2L, 12L, 4L,   # EDA - Master
    2L, 16L, 3L,   # linear_models - Advanced
    2L, 17L, 3L,   # inference_prediction - Advanced
    2L, 18L, 3L,   # statistical_learning - Advanced
    2L, 14L, 3L,   # feature_engineering - Advanced
    2L,  3L, 3L,   # problem_formulation - Advanced
    2L,  5L, 3L,   # data_narratives - Advanced
    2L,  4L, 3L,   # limits - Advanced

    # AI/ML Specialist
    3L,  1L, 4L,   # algorithms - Master
    3L,  2L, 4L,   # programming - Master
    3L,  8L, 3L,   # linear_algebra - Advanced
    3L, 10L, 3L,   # optimization - Advanced
    3L, 18L, 4L,   # statistical_learning - Master
    3L, 14L, 3L,   # feature_engineering - Advanced
    3L,  9L, 2L    # graph_theory - Intermediate
  )

  # =========================================================================
  # EDGE TYPE 3: prerequisite (Skill → Skill)
  #   Directed edge: skill_from must be learned before skill_to
  #
  # Design notes: Create a plausible learning DAG
  #   - Math foundations → Statistical foundations → Applied skills
  #   - Programming is prerequisite for many applied skills
  #   - Applied track connects to ML via feature_engineering
  #   - Cross-track bridges ensure reasonable connectivity
  #
  # v1.0.2: 26 edges total
  #   - 22 original edges
  #   - 1 bridge: feature_engineering → statistical_learning (v1.0.1)
  #   - 3 new bridges: data_collection → data_cleaning,
  #                    EDA → experimental_design,
  #                    optimization → statistical_learning
  # =========================================================================
  prerequisite <- tibble::tribble(
    ~skill_from_id, ~skill_to_id,
    # Linear algebra is foundational
    8L, 10L,   # linear_algebra → optimization
    8L, 16L,   # linear_algebra → linear_models
    8L, 18L,   # linear_algebra → statistical_learning

    # Probability is foundational
    7L, 17L,   # probability_theory → inference_prediction
    7L, 15L,   # probability_theory → experimental_design
    7L, 18L,   # probability_theory → statistical_learning

    # Programming enables applied work
    2L, 12L,   # programming → EDA
    2L, 11L,   # programming → data_cleaning
    2L,  1L,   # programming → algorithms
    2L, 14L,   # programming → feature_engineering

    # Data engineering track (v1.0.2: connects Data Collection)
    6L, 11L,   # data_collection → data_cleaning

    # EDA is central
    11L, 12L,  # data_cleaning → EDA
    12L, 14L,  # EDA → feature_engineering
    12L, 13L,  # EDA → data_visualization

    # Statistical chain
    16L, 17L,  # linear_models → inference_prediction
    17L, 18L,  # inference_prediction → statistical_learning

    # Bridge: applied track → ML (v1.0.1)
    14L, 18L,  # feature_engineering → statistical_learning

    # Bridge: math track → ML (v1.0.2)
    10L, 18L,  # optimization → statistical_learning

    # Bridge: applied → stats theory (v1.0.2)
    12L, 15L,  # EDA → experimental_design

    # Graph theory path
    8L,  9L,   # linear_algebra → graph_theory (adjacency matrices)
    1L,  9L,   # algorithms → graph_theory (graph algorithms)

    # Collaboration skills
    12L,  4L,  # EDA → limits
    13L,  5L,  # data_visualization → data_narratives
    4L,  5L,   # limits → data_narratives
    12L,  3L,  # EDA → problem_formulation
    4L,  3L    # limits → problem_formulation
  )

  # =========================================================================
  # EDGE TYPE 4: teaches (Course → Skill)
  #   Which skills does each course develop?
  # =========================================================================
  teaches <- tibble::tribble(
    ~course_id, ~skill_id, ~skill_level_taught,
    # Statistical Methods and Data Analysis
    1L,  7L, 2L,   # probability_theory - Intermediate
    1L, 15L, 2L,   # experimental_design - Intermediate
    1L, 16L, 3L,   # linear_models - Advanced
    1L, 17L, 3L,   # inference_prediction - Advanced
    1L, 12L, 3L,   # EDA - Advanced

    # Algorithms for Data Science
    2L,  1L, 3L,   # algorithms - Advanced
    2L,  2L, 2L,   # programming - Intermediate
    2L,  9L, 2L,   # graph_theory - Intermediate
    2L, 10L, 2L,   # optimization - Intermediate

    # Data Patterns and Representations
    3L,  8L, 3L,   # linear_algebra - Advanced
    3L, 14L, 3L,   # feature_engineering - Advanced
    3L, 18L, 3L,   # statistical_learning - Advanced

    # Data Engineering Principles
    4L,  6L, 3L,   # data_collection - Advanced
    4L, 11L, 3L,   # data_cleaning - Advanced
    4L,  2L, 2L,   # programming - Intermediate

    # Foundations of Agentic AI
    5L,  1L, 2L,   # algorithms - Intermediate
    5L,  3L, 2L,   # problem_formulation - Intermediate

    # Machine Learning Foundations
    6L,  8L, 2L,   # linear_algebra - Intermediate
    6L,  7L, 2L,   # probability_theory - Intermediate
    6L, 18L, 3L,   # statistical_learning - Advanced
    6L, 10L, 2L    # optimization - Intermediate
  )

  # =========================================================================
  # EDGE TYPE 5: skill_in_competency (Skill → Competency)
  #   Already encoded in skills$cmp_id, but explicit for graph construction
  # =========================================================================
  skill_in_competency <- skills |>
    dplyr::select(skill_id, cmp_id) |>
    dplyr::rename(competency_id = cmp_id)

  # =========================================================================
  # Assemble the LearningGraph
  # =========================================================================
  learning_graph <- list(
    # Metadata
    metadata = list(
      name = "LearningGraph",
      description = "A knowledge graph for skills-based learning in data science",
      source = "Based on IC Data Science CRG (2023) and Workera.ai concepts",
      version = "1.0.2",
      created = Sys.Date()
    ),

    # Reference table for proficiency levels
    proficiency_levels = proficiency_levels,

    # Node tables (vertices)
    nodes = list(
      competencies = competencies,
      skills       = skills,
      work_roles   = work_roles,
      courses      = courses,
      learners     = learners
    ),

    # Edge tables (relationships)
    edges = list(
      has_skill           = has_skill,            # Learner → Skill (weighted)
      requires_skill      = requires_skill,       # WorkRole → Skill (weighted)
      prerequisite        = prerequisite,         # Skill → Skill (directed)
      teaches             = teaches,              # Course → Skill (weighted)
      skill_in_competency = skill_in_competency   # Skill → Competency
    )
  )

  return(learning_graph)
}


## 
#  lg_to_igraph()
## 
#' Convert LearningGraph to igraph object
#'
#' @param lg A LearningGraph list object
#' @param edge_type Which edge type to use: "prerequisite", "has_skill", etc.
#' @return An igraph object
#' @export
lg_to_igraph <- function(lg, edge_type = "prerequisite") {

  if (edge_type == "prerequisite") {
    # Skill → Skill graph
    edges <- lg$edges$prerequisite
    nodes <- lg$nodes$skills

    g <- igraph::graph_from_data_frame(
      d = edges |>
        dplyr::left_join(
          nodes |> dplyr::select(skill_id, skill_name),
          by = c("skill_from_id" = "skill_id")
        ) |>
        dplyr::rename(from = skill_name) |>
        dplyr::left_join(
          nodes |> dplyr::select(skill_id, skill_name),
          by = c("skill_to_id" = "skill_id")
        ) |>
        dplyr::rename(to = skill_name) |>
        dplyr::select(from, to),
      directed = TRUE,
      vertices = nodes |> dplyr::select(name = skill_name, skill_id, cmp_id, k_or_s)
    )

  } else if (edge_type == "skill_competency") {
    # Bipartite: Skills + Competencies
    skill_nodes <- lg$nodes$skills |>
      dplyr::transmute(name = skill_name, type = "skill", id = skill_id)
    comp_nodes <- lg$nodes$competencies |>
      dplyr::transmute(name = cmp_abbrev, type = "competency", id = cmp_id)
    all_nodes <- dplyr::bind_rows(skill_nodes, comp_nodes)

    edges <- lg$edges$skill_in_competency |>
      dplyr::left_join(
        lg$nodes$skills |> dplyr::select(skill_id, skill_name),
        by = "skill_id"
      ) |>
      dplyr::left_join(
        lg$nodes$competencies |> dplyr::select(cmp_id, cmp_abbrev),
        by = c("competency_id" = "cmp_id")
      ) |>
      dplyr::transmute(from = skill_name, to = cmp_abbrev)

    g <- igraph::graph_from_data_frame(d = edges, directed = FALSE, vertices = all_nodes)

  } else if (edge_type == "learner_skill") {
    # Bipartite: Learners + Skills
    learner_nodes <- lg$nodes$learners |>
      dplyr::transmute(name = name, type = "learner", id = learner_id)
    skill_nodes <- lg$nodes$skills |>
      dplyr::transmute(name = skill_name, type = "skill", id = skill_id)
    all_nodes <- dplyr::bind_rows(learner_nodes, skill_nodes)

    edges <- lg$edges$has_skill |>
      dplyr::left_join(
        lg$nodes$learners |> dplyr::select(learner_id, name),
        by = "learner_id"
      ) |>
      dplyr::rename(from = name) |>
      dplyr::left_join(
        lg$nodes$skills |> dplyr::select(skill_id, skill_name),
        by = "skill_id"
      ) |>
      dplyr::rename(to = skill_name) |>
      dplyr::select(from, to, proficiency)

    g <- igraph::graph_from_data_frame(d = edges, directed = FALSE, vertices = all_nodes)
    igraph::E(g)$weight <- edges$proficiency

  } else {
    stop("Unknown edge_type: ", edge_type)
  }

  return(g)
}


## 
#  lg_skill_gap()
## 
#' Compute skill gap for a learner targeting a work role
#'
#' @param lg LearningGraph object
#' @param learner_name Name of the learner
#' @param role_tag Tag of the target work role
#' @return A tibble showing skill gaps
#' @export
lg_skill_gap <- function(lg, learner_name, role_tag) {
  # Get learner's current skills
  learner_id_val <- lg$nodes$learners |>
    dplyr::filter(name == learner_name) |>
    dplyr::pull(learner_id)

  current <- lg$edges$has_skill |>
    dplyr::filter(learner_id == learner_id_val) |>
    dplyr::select(skill_id, current_level = proficiency)

  # Get role requirements
  role_id_val <- lg$nodes$work_roles |>
    dplyr::filter(role_tag == !!role_tag) |>
    dplyr::pull(role_id)

  required <- lg$edges$requires_skill |>
    dplyr::filter(role_id == role_id_val) |>
    dplyr::select(skill_id, required_level = required_proficiency)

  # Compute gap
  gap <- required |>
    dplyr::left_join(current, by = "skill_id") |>
    dplyr::mutate(current_level = tidyr::replace_na(current_level, 0L)) |>
    dplyr::mutate(gap = required_level - current_level) |>
    dplyr::left_join(
      lg$nodes$skills |> dplyr::select(skill_id, skill_name),
      by = "skill_id"
    ) |>
    dplyr::select(skill_name, current_level, required_level, gap) |>
    dplyr::arrange(dplyr::desc(gap))

  return(gap)
}


## 
#  lg_learning_path()
## 
#' Find shortest learning path between two skills
#'
#' @param lg LearningGraph object
#' @param from_skill Starting skill name
#' @param to_skill Target skill name
#' @return Character vector of skill names in path order
#' @export
lg_learning_path <- function(lg, from_skill, to_skill) {

  g <- lg_to_igraph(lg, "prerequisite")

  # Note: prerequisite graph has edges pointing TO advanced skills
  # So we need to find path from from_skill to to_skill
  path <- igraph::shortest_paths(g, from = from_skill, to = to_skill, output = "vpath")

  if (length(path$vpath[[1]]) == 0) {
    return(paste("No path from", from_skill, "to", to_skill))
  }

  return(names(path$vpath[[1]]))
}


## 
#  run_lg_examples()
## 
#' Build and inspect the graph
#'
#' @return A list containing nodes (by type) and edges (by relation type)
run_lg_examples <- function() {
  lg <- build_learning_graph()
  
  # Summary
  cat("LearningGraph Summary (v1.0.2)\n")
  cat("==============================\n")
  cat("Nodes:\n")
  cat("  Competencies:", nrow(lg$nodes$competencies), "\n")
  cat("  Skills:      ", nrow(lg$nodes$skills), "\n")
  cat("  Work Roles:  ", nrow(lg$nodes$work_roles), "\n")
  cat("  Courses:     ", nrow(lg$nodes$courses), "\n")
  cat("  Learners:    ", nrow(lg$nodes$learners), "\n")
  cat("\nEdges:\n")
  cat("  has_skill:          ", nrow(lg$edges$has_skill), "\n")
  cat("  requires_skill:     ", nrow(lg$edges$requires_skill), "\n")
  cat("  prerequisite:       ", nrow(lg$edges$prerequisite), "\n")
  cat("  teaches:            ", nrow(lg$edges$teaches), "\n")
  cat("  skill_in_competency:", nrow(lg$edges$skill_in_competency), "\n")
  
  # Example: Alice's gap for Data Scientist role
  cat("\n\nAlice's skill gap for Data Scientist role:\n")
  print(lg_skill_gap(lg, "Alice", "DSci"))
  
  # Example: Learning paths demonstrating connectivity
  cat("\n\nLearning path from Programming to Statistical Learning:\n")
  print(lg_learning_path(lg, "Programming", "Statistical Learning"))
  
  cat("\n\nLearning path from Data Collection to Data Narratives:\n")
  print(lg_learning_path(lg, "Data Collection", "Data Narratives"))
  
  cat("\n\nLearning path from Programming to Experimental Design:\n")
  print(lg_learning_path(lg, "Programming", "Experimental Design"))
  
  return(lg)
}

##
#  EOF
##
