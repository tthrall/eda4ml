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
#       1.0.3 - Added profile-based learning path functions:
#               lg_learning_path(), lg_learning_path_detail(),
#               lg_compare_paths(), lg_plot_learning_path()
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
      version = "1.0.3",
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
#' Find the minimal learning path from a learner's current profile to a target skill
#'
#' Given a learner's current skill profile, computes the sequence of skills
#' that must be acquired to reach a target skill, respecting all prerequisite
#' dependencies.
#'
#' @param lg A learning_graph object from eda4mldata
#' @param learner_id Integer ID of the learner (1-6 in current data)
#' @param target_skill Character name of the target skill
#' @param include_target Logical; include the target skill in the returned path?
#'
#' @return A character vector of skill names in valid learning order
#'   (topologically sorted so prerequisites come first)
#'
#' @details
#' The algorithm:
#' 1. Retrieves the learner's current skill set
#' 2. Finds all transitive prerequisites of the target skill
#' 3. Filters to skills the learner doesn't yet have
#' 4. Returns these in topological order (a valid acquisition sequence)
#'
#' This is NOT exponentially complex because we search the prerequisite DAG,
#' not the space of all possible profiles. Complexity is O(V + E) where V and E
#' are the vertices and edges of the prerequisite subgraph.
#'
#' @examples
#' lg <- build_learning_graph()
#' # What must Alice learn to reach Statistical Learning?
#' lg_learning_path(lg, learner_id = 1, target_skill = "Statistical Learning")
#'
#' @export
lg_learning_path <- function(lg, learner_id, target_skill, include_target = TRUE) {
  
  # Validate inputs
  if (!learner_id %in% lg$nodes$learners$learner_id) {
    stop("learner_id ", learner_id, " not found. Valid IDs: ",
         paste(lg$nodes$learners$learner_id, collapse = ", "))
  }
  
  # Get target skill ID
  target_row <- lg$nodes$skills |>
    dplyr::filter(skill_name == target_skill)
  
  if (nrow(target_row) == 0) {
    valid_skills <- lg$nodes$skills$skill_name
    stop("Target skill '", target_skill, "' not found.\n",
         "Valid skills: ", paste(valid_skills, collapse = ", "))
  }
  target_id <- target_row$skill_id
  
  # Get learner's current skill IDs
  current_skill_ids <- lg$edges$has_skill |>
    dplyr::filter(learner_id == !!learner_id) |>
    dplyr::pull(skill_id)
  
  # Check if learner already has the target skill
  if (target_id %in% current_skill_ids) {
    learner_name <- lg$nodes$learners |>
      dplyr::filter(learner_id == !!learner_id) |>
      dplyr::pull(name)
    message(learner_name, " already has skill: ", target_skill)
    return(character(0))
  }
  
  # Build prerequisite graph using skill_id as vertex name
  prereq_edges <- lg$edges$prerequisite |>
    dplyr::transmute(
      from = as.character(skill_from_id),
      to = as.character(skill_to_id)
    )
  
  # Create vertex data frame with skill_id as name
  vertices <- lg$nodes$skills |>
    dplyr::transmute(
      name = as.character(skill_id),
      skill_name = skill_name
    )
  
  g <- igraph::graph_from_data_frame(
    prereq_edges,
    directed = TRUE,
    vertices = vertices
  )
  
  # Find all ancestors of target (transitive prerequisites)
  # mode = "in" gets vertices FROM WHICH target is reachable
  # i.e., all prerequisites that lead to the target
  ancestors <- igraph::subcomponent(g, as.character(target_id), mode = "in")
  ancestor_ids <- as.integer(names(ancestors))
  
  # Determine which skills are needed
  if (include_target) {
    all_needed_ids <- ancestor_ids
  } else {
    all_needed_ids <- setdiff(ancestor_ids, target_id)
  }
  
  # Filter to skills the learner doesn't have
  missing_ids <- setdiff(all_needed_ids, current_skill_ids)
  
  if (length(missing_ids) == 0) {
    return(character(0))
  }
  
  # Induce subgraph on missing skills
  g_missing <- igraph::induced_subgraph(g, as.character(missing_ids))
  
  # Topological sort gives a valid learning order
  # mode = "out" processes from sources (entry skills) to sinks (advanced skills)
  topo_order <- igraph::topo_sort(g_missing, mode = "out")
  
  # Extract skill names in order
  skill_names <- igraph::vertex_attr(g_missing, "skill_name", index = topo_order)
  
  return(skill_names)
}


## 
#  lg_learning_path_detail()
## 
#' Get detailed learning path information
#'
#' Returns a tibble with full details about each skill in the learning path,
#' including competency membership and whether courses are available.
#'
#' @inheritParams lg_learning_path
#' @return A tibble with columns: step, skill_id, skill_name, cmp_id,
#'   cmp_name, courses_available
#'
#' @examples
#' lg <- build_learning_graph()
#' lg_learning_path_detail(lg, learner_id = 1, 
#'                         target_skill = "Statistical Learning")
#'
#' @export
lg_learning_path_detail <- function(lg, learner_id, target_skill, include_target = TRUE) {
  
  # Get the basic path
  path_skills <- lg_learning_path(lg, learner_id, target_skill, include_target)
  
  if (length(path_skills) == 0) {
    return(tibble::tibble(
      step = integer(),
      skill_id = integer(),
      skill_name = character(),
      cmp_id = integer(),
      cmp_name = character(),
      courses_available = integer()
    ))
  }
  
  # Build detail tibble
  skill_info <- lg$nodes$skills |>
    dplyr::filter(skill_name %in% path_skills)
  
  # Note: competencies table uses cmp_id and cmp_name
  competency_info <- lg$nodes$competencies |>
    dplyr::select(cmp_id, cmp_name)
  
  # Count courses teaching each skill
  courses_per_skill <- lg$edges$teaches |>
    dplyr::count(skill_id, name = "courses_available")
  
  result <- tibble::tibble(skill_name = path_skills) |>
    dplyr::mutate(step = dplyr::row_number()) |>
    dplyr::left_join(skill_info, by = "skill_name") |>
    dplyr::left_join(competency_info, by = "cmp_id") |>
    dplyr::left_join(courses_per_skill, by = "skill_id") |>
    dplyr::mutate(courses_available = tidyr::replace_na(courses_available, 0L)) |>
    dplyr::select(step, skill_id, skill_name, cmp_id, cmp_name, courses_available)
  
  return(result)
}


## 
#  lg_compare_paths()
## 
#' Compare learning paths for multiple learners
#'
#' Shows how different starting profiles lead to different paths to the same goal.
#'
#' @param lg A learning_graph object
#' @param learner_ids Integer vector of learner IDs to compare
#' @param target_skill Character name of target skill
#'
#' @return A tibble with learner_id, learner_name, path_length, and path (as list column)
#'
#' @examples
#' lg <- build_learning_graph()
#' lg_compare_paths(lg, learner_ids = 1:3, target_skill = "Statistical Learning")
#'
#' @export
lg_compare_paths <- function(lg, learner_ids, target_skill) {
  
  results <- purrr::map_dfr(learner_ids, function(lid) {
    learner_name <- lg$nodes$learners |>
      dplyr::filter(learner_id == lid) |>
      dplyr::pull(name)
    
    path <- lg_learning_path(lg, lid, target_skill, include_target = TRUE)
    
    tibble::tibble(
      learner_id = lid,
      learner_name = learner_name,
      path_length = length(path),
      path = list(path)
    )
  })
  
  return(results)
}


## 
#  lg_plot_learning_path()
## 
#' Visualize a learning path on the prerequisite DAG
#'
#' Highlights the skills a learner must acquire, showing current skills,
#' required skills, and the target.
#'
#' @param lg A learning_graph object
#' @param learner_id Integer learner ID
#' @param target_skill Character name of target skill
#' @param ... Additional arguments passed to plot()
#'
#' @return Invisible; called for side effect of plotting
#'
#' @export
lg_plot_learning_path <- function(lg, learner_id, target_skill, ...) {
  
  # Get learner info
  learner_name <- lg$nodes$learners |>
    dplyr::filter(learner_id == !!learner_id) |>
    dplyr::pull(name)
  
  current_skill_ids <- lg$edges$has_skill |>
    dplyr::filter(learner_id == !!learner_id) |>
    dplyr::pull(skill_id)
  
  # Get learning path
  path_skills <- lg_learning_path(lg, learner_id, target_skill, include_target = TRUE)
  path_ids <- lg$nodes$skills |>
    dplyr::filter(skill_name %in% path_skills) |>
    dplyr::pull(skill_id)
  
  target_id <- lg$nodes$skills |>
    dplyr::filter(skill_name == target_skill) |>
    dplyr::pull(skill_id)
  
  # Build full prerequisite graph
  prereq_edges <- lg$edges$prerequisite |>
    dplyr::left_join(
      lg$nodes$skills |> dplyr::select(skill_from_id = skill_id, from = skill_name),
      by = "skill_from_id"
    ) |>
    dplyr::left_join(
      lg$nodes$skills |> dplyr::select(skill_to_id = skill_id, to = skill_name),
      by = "skill_to_id"
    ) |>
    dplyr::select(from, to)
  
  g <- igraph::graph_from_data_frame(prereq_edges, directed = TRUE)
  
  # Color vertices by status
  skill_status <- lg$nodes$skills |>
    dplyr::mutate(
      status = dplyr::case_when(
        skill_id == target_id ~ "target",
        skill_id %in% current_skill_ids ~ "current",
        skill_id %in% path_ids ~ "to_learn",
        TRUE ~ "other"
      )
    )
  
  status_colors <- c(
    "current" = "#2ECC71",   # green - already know
    "to_learn" = "#E74C3C", # red - must learn
    "target" = "#9B59B6",   # purple - goal
    "other" = "#BDC3C7"     # gray - not relevant
  )
  
  vertex_status <- skill_status$status[match(igraph::V(g)$name, skill_status$skill_name)]
  igraph::V(g)$color <- status_colors[vertex_status]
  igraph::V(g)$size <- ifelse(vertex_status == "other", 8, 15)
  
  # Highlight edges on the path
  edge_list <- igraph::as_edgelist(g)
  edge_on_path <- (edge_list[,1] %in% path_skills) & (edge_list[,2] %in% path_skills)
  igraph::E(g)$color <- ifelse(edge_on_path, "#E74C3C", "gray70")
  igraph::E(g)$width <- ifelse(edge_on_path, 2, 1)
  
  # Plot
  set.seed(42)
  plot(g,
       layout = igraph::layout_with_sugiyama(g)$layout,
       vertex.label.cex = 0.6,
       vertex.label.color = "black",
       edge.arrow.size = 0.4,
       main = paste0(learner_name, "'s path to ", target_skill),
       ...)
  
  # Legend
  legend("bottomright",
         legend = c("Current skills", "Must learn", "Target", "Not needed"),
         fill = status_colors[c("current", "to_learn", "target", "other")],
         bty = "n",
         cex = 0.8)
  
  invisible(g)
}


## 
#  lg_learning_path_OLD()
## 
#' Find shortest learning path between two skills (DEPRECATED)
#'
#' @param lg LearningGraph object
#' @param from_skill Starting skill name
#' @param to_skill Target skill name
#' @return Character vector of skill names in path order
#' @export
lg_learning_path_OLD <- function(lg, from_skill, to_skill) {
  
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
  cat("LearningGraph Summary (v1.0.3)\n")
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
  
  # Example: Learning paths demonstrating the profile-based approach
  cat("\n\nAlice's learning path to Statistical Learning:\n")
  print(lg_learning_path(lg, learner_id = 1, target_skill = "Statistical Learning"))
  
  cat("\n\nCharlie's learning path to Data Narratives:\n")
  print(lg_learning_path(lg, learner_id = 3, target_skill = "Data Narratives"))
  
  cat("\n\nDan's learning path to Experimental Design:\n")
  print(lg_learning_path(lg, learner_id = 4, target_skill = "Experimental Design"))
  
  return(lg)
}

##
#  EOF
##
