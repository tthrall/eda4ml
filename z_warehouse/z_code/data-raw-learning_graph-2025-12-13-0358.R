## code to prepare `learning_graph` dataset for eda4mldata

# =============================================================================
# LearningGraph: A knowledge graph for skills-based learning in data science
#
# Source: Based on IC Data Science Competency Resource Guide (CRG) 2023
#         Office of the Director of National Intelligence
#         https://www.dni.gov/ (UNCLASSIFIED)
#
# Inspiration: Workera.ai skills intelligence platform
#              https://workera.ai/
#
# Purpose: Demonstrate graph theory concepts in Chapter 15 (Graph Theory for ML)
#          - Directed acyclic graphs (prerequisite structure)
#          - Bipartite graphs (learner-skill, course-skill)
#          - Weighted edges (proficiency levels)
#          - Path algorithms (shortest learning path)
#          - Gap analysis (current vs. required skills)
# =============================================================================

# -----------------------------------------------------------------------------
# Proficiency Levels (from CRG Appendix A, p.12)
# -----------------------------------------------------------------------------
lg_proficiency_levels <- tibble::tribble(
  ~level, ~label,         ~guidance_needed,             ~development_focus,
  0L,     "None",         "N/A",                        "N/A",
  1L,     "Basic",        "Frequently",                 "Learning established methods for routine situations",
  2L,     "Intermediate", "Occasionally",               "Depth to address difficult, novel situations",
  3L,     "Advanced",     "Rarely",                     "Blending skills for complex, ambiguous situations",
  4L,     "Master",       "None (recognized authority)","Continuing education for breadth and currency"
)

# -----------------------------------------------------------------------------
# Competencies (all 7 from CRG, Table 2, p.4)
# -----------------------------------------------------------------------------
lg_competencies <- tibble::tribble(
  ~cmp_id, ~cmp_tag,        ~cmp_name,                                       ~cmp_abbrev,
  1L,      "compute_fndns", "Computational Foundations of Data Science",     "Computation",
  2L,      "collab",        "Cross-Disciplinary Data Science Collaboration", "Collaboration",
  3L,      "data_engr",     "Data Engineering for Data Science",             "Data Engineering",
  4L,      "ds_ai_gov",     "Data Science/AI Governance and Ethics",         "Governance",
  5L,      "math_fndns",    "Mathematical Foundations of Data Science",      "Math",
  6L,      "data_analysis", "Scientific Data Analysis",                      "Analysis",
  7L,      "stats_fndns",   "Statistical Foundations of Data Science",       "Statistics"
)

# -----------------------------------------------------------------------------
# Skills (KSAs) - Curated subset from CRG Table 3 (pp.5-11)
#
# Selection criteria for textbook scope:
#   - Focus on competencies 5, 6, 7 (Math, Analysis, Statistics)
#   - Include key computational skills (programming, algorithms)
#   - Include collaboration skills for data narratives
#   - 18 skills for manageable visualization
# -----------------------------------------------------------------------------
lg_skills <- tibble::tribble(
  ~skill_id, ~skill_tag,           ~skill_name,               ~cmp_id, ~k_or_s, ~description,
  # Computational Foundations (cmp_id = 1)
  1L,  "algorithms",         "Algorithms",                   1L, "k", "Knowledge of designing and implementing algorithms, from algorithmic thinking through ML methods",
  2L,  "programming",        "Programming",                  1L, "s", "Skill in programming in compiled and interpreted languages with software development practices",

  # Collaboration (cmp_id = 2)
  3L,  "problem_formulation","Problem Formulation",          2L, "s", "Skill in approximating domain problems with data science questions",
  4L,  "limits",             "Limitations",                  2L, "s", "Skill in communicating the limitations of data and models",
  5L,  "data_narratives",    "Data Narratives",              2L, "s", "Skill in building data narratives that communicate principled inferences",

  # Data Engineering (cmp_id = 3)
  6L,  "data_collection",    "Data Collection",              3L, "s", "Skill in gathering structured or unstructured datasets",

  # Math Foundations (cmp_id = 5)
  7L,  "probability_theory", "Probability Theory",           5L, "k", "Knowledge of probability theory, from independence to stochastic processes",
  8L,  "linear_algebra",     "Linear Algebra",               5L, "k", "Knowledge of vectors, matrices, abstract vector spaces, and numerical methods",
  9L,  "graph_theory",       "Graph Theory",                 5L, "k", "Knowledge of nodes, edges, and algorithmic solutions like shortest path",
  10L, "optimization",       "Optimization",                 5L, "k", "Knowledge of mathematical optimization, from calculus to constrained problems",

  # Scientific Data Analysis (cmp_id = 6)
  11L, "data_cleaning",      "Data Cleaning",                6L, "s", "Skill in preparing data by handling missing or low-quality records",
  12L, "EDA",                "Exploratory Data Analysis",    6L, "s", "Skill in iterative visualization, summarization, and unsupervised learning",
  13L, "data_visualization", "Data Visualization",           6L, "s", "Skill in displaying data to enable comparisons and enhance comprehension",
  14L, "feature_engineering","Feature Engineering",          6L, "s", "Skill in transforming data guided by domain knowledge",

  # Statistical Foundations (cmp_id = 7)
  15L, "experimental_design","Experimental Design",          7L, "k", "Knowledge of designing surveys, experiments, and observational studies",
  16L, "linear_models",      "Linear Models",                7L, "k", "Knowledge from simple linear models through generalized linear models",
  17L, "inference_prediction","Inference and Prediction",    7L, "k", "Knowledge of estimation, predictive inference, hypothesis testing, simulation",
  18L, "statistical_learning","Statistical Learning",        7L, "k", "Knowledge of algorithms from nearest-neighbor to neural nets"
)

# -----------------------------------------------------------------------------
# Work Roles - Curated subset from CDAO work roles
# -----------------------------------------------------------------------------
lg_work_roles <- tibble::tribble(
  ~role_id, ~role_tag, ~role_name,        ~role_description,
  1L, "DA",    "Data Analyst",      "Analyzes data and builds visualizations to report insights",
  2L, "DSci",  "Data Scientist",    "Combines scientific method, math, programming, and storytelling",
  3L, "AI_ML", "AI/ML Specialist",  "Designs and develops AI applications and solutions"
)

# -----------------------------------------------------------------------------
# Courses - Selected to align with EDA4ML textbook chapters
# -----------------------------------------------------------------------------
lg_courses <- tibble::tribble(
  ~course_id, ~course_tag,     ~course_name,                           ~provider,
  1L, "stat_methods",   "Statistical Methods and Data Analysis", "JHU",
  2L, "algo_ds",        "Algorithms for Data Science",           "JHU",
  3L, "data_patterns",  "Data Patterns and Representations",     "JHU",
  4L, "data_engr",      "Data Engineering Principles",           "JHU",
  5L, "agent_fndns",    "Foundations of Agentic AI",             "JHU",
  6L, "ml_fndns",       "Machine Learning Foundations",          "Coursera"
)

# -----------------------------------------------------------------------------
# Learners - Fictional profiles with varying backgrounds
# -----------------------------------------------------------------------------
lg_learners <- tibble::tribble(
  ~learner_id, ~name,    ~role,      ~organization, ~background,
  1L, "Alice",   "student",  "Xavier U",    "Math major, junior year",
  2L, "Beth",    "employee", "DataCorp",    "Senior analyst, 5 years experience",
  3L, "Charlie", "student",  "Xavier U",    "CS major, senior year",
  4L, "Dan",     "student",  "Xavier U",    "Statistics minor, interested in ML",
  5L, "Elliot",  "employee", "DataCorp",    "Junior data engineer, 1 year",
  6L, "Fiona",   "employee", "DataCorp",    "Mid-level data scientist, 3 years"
)

# -----------------------------------------------------------------------------
# Edge: has_skill (Learner → Skill, weighted by proficiency)
# -----------------------------------------------------------------------------
lg_has_skill <- tibble::tribble(
  ~learner_id, ~skill_id, ~proficiency,
  # Alice - Math major (strong theory, weak applied)
  1L,  7L, 3L,   # probability_theory - Advanced
  1L,  8L, 3L,   # linear_algebra - Advanced
  1L, 10L, 2L,   # optimization - Intermediate
  1L,  2L, 1L,   # programming - Basic
  1L, 12L, 1L,   # EDA - Basic
  1L, 16L, 2L,   # linear_models - Intermediate

  # Beth - Senior analyst (strong applied, moderate theory)
  2L,  2L, 3L,   # programming - Advanced
  2L, 11L, 4L,   # data_cleaning - Master
  2L, 12L, 4L,   # EDA - Master
  2L, 13L, 4L,   # data_visualization - Master
  2L,  5L, 3L,   # data_narratives - Advanced
  2L,  8L, 2L,   # linear_algebra - Intermediate
  2L, 16L, 3L,   # linear_models - Advanced
  2L, 15L, 2L,   # experimental_design - Intermediate

  # Charlie - CS major (strong programming, weak statistics)
  3L,  1L, 3L,   # algorithms - Advanced
  3L,  2L, 4L,   # programming - Master
  3L,  9L, 2L,   # graph_theory - Intermediate
  3L,  8L, 2L,   # linear_algebra - Intermediate
  3L, 12L, 1L,   # EDA - Basic
  3L, 18L, 2L,   # statistical_learning - Intermediate

  # Dan - Statistics minor (balanced, moderate levels)
  4L,  7L, 2L,   # probability_theory - Intermediate
  4L,  8L, 2L,   # linear_algebra - Intermediate
  4L, 12L, 2L,   # EDA - Intermediate
  4L, 16L, 2L,   # linear_models - Intermediate
  4L, 17L, 2L,   # inference_prediction - Intermediate
  4L,  2L, 1L,   # programming - Basic

  # Elliot - Junior data engineer (data handling focus)
  5L,  2L, 2L,   # programming - Intermediate
  5L,  6L, 3L,   # data_collection - Advanced
  5L, 11L, 3L,   # data_cleaning - Advanced
  5L, 14L, 2L,   # feature_engineering - Intermediate
  5L, 12L, 1L,   # EDA - Basic

  # Fiona - Mid-level data scientist (well-rounded)
  6L,  2L, 3L,   # programming - Advanced
  6L,  7L, 3L,   # probability_theory - Advanced
  6L,  8L, 3L,   # linear_algebra - Advanced
  6L, 12L, 3L,   # EDA - Advanced
  6L, 13L, 3L,   # data_visualization - Advanced
  6L, 16L, 3L,   # linear_models - Advanced
  6L, 17L, 3L,   # inference_prediction - Advanced
  6L, 18L, 3L,   # statistical_learning - Advanced
  6L,  3L, 2L,   # problem_formulation - Intermediate
  6L,  5L, 2L    # data_narratives - Intermediate
)

# -----------------------------------------------------------------------------
# Edge: requires_skill (Work Role → Skill, weighted by required proficiency)
# -----------------------------------------------------------------------------
lg_requires_skill <- tibble::tribble(
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

# -----------------------------------------------------------------------------
# Edge: prerequisite (Skill → Skill, directed)
# -----------------------------------------------------------------------------
lg_prerequisite <- tibble::tribble(
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

  # EDA is central
  11L, 12L,  # data_cleaning → EDA
  12L, 14L,  # EDA → feature_engineering
  12L, 13L,  # EDA → data_visualization

  # Statistical chain
  16L, 17L,  # linear_models → inference_prediction
  17L, 18L,  # inference_prediction → statistical_learning

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

# -----------------------------------------------------------------------------
# Edge: teaches (Course → Skill, weighted by level taught)
# -----------------------------------------------------------------------------
lg_teaches <- tibble::tribble(
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

# -----------------------------------------------------------------------------
# Schema Definition
#
# The schema specifies permissible node types, edge types, and constraints.
# It travels with the data to enable validation and documentation.
# -----------------------------------------------------------------------------
lg_schema <- tibble::tribble(
  ~edge_type,            ~source_type, ~target_type,  ~property,     ~property_semantics,
 "has_skill",           "learner",    "skill",       "proficiency", "current_level",
  "requires_skill",      "work_role",  "skill",       "proficiency", "minimum_threshold",
  "prerequisite",        "skill",      "skill",       NA_character_, NA_character_,
  "teaches",             "course",     "skill",       "proficiency", "maximum_ceiling",
  "skill_in_competency", "skill",      "competency",  NA_character_, NA_character_
)

# Note on skill_in_competency: This relationship is normalized into the skills
# table via the cmp_id column rather than stored as a separate edge table.
# This design choice reduces redundancy since each skill belongs to exactly
# one competency. The schema documents it as a conceptual edge type for
# completeness.

# -----------------------------------------------------------------------------
# Assemble the LearningGraph as a single list object
# -----------------------------------------------------------------------------
learning_graph <- list(
  # Metadata
  metadata = list(
    name = "LearningGraph",
    description = "A knowledge graph for skills-based learning in data science",
    source = "Based on IC Data Science CRG (2023), UNCLASSIFIED",
    reference = "Office of the Director of National Intelligence",
    inspiration = "Workera.ai skills intelligence platform",
    version = "1.0.2",
    schema = lg_schema
  ),

  # Reference
  proficiency_levels = lg_proficiency_levels,

  # Nodes
  nodes = list(
    competencies = lg_competencies,
    skills       = lg_skills,
    work_roles   = lg_work_roles,
    courses      = lg_courses,
    learners     = lg_learners
  ),

  # Edges
  edges = list(
    has_skill      = lg_has_skill,
    requires_skill = lg_requires_skill,
    prerequisite   = lg_prerequisite,
    teaches        = lg_teaches
  )
)

# Save to package data directory
usethis::use_data(learning_graph, overwrite = TRUE)

# Also export individual components for convenience
usethis::use_data(
  lg_proficiency_levels,
  lg_competencies,
  lg_skills,
  lg_work_roles,
  lg_courses,
  lg_learners,
  lg_has_skill,
  lg_requires_skill,
  lg_prerequisite,
  lg_teaches,
  lg_schema,
  overwrite = TRUE
)
