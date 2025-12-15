#' LearningGraph: A Knowledge Graph for Skills-Based Learning
#'
#' A multi-relational knowledge graph representing the data science skills
#' ecosystem, designed for demonstrating graph theory concepts in machine
#' learning applications.
#'
#' @format A list with the following components:
#' \describe{
#'   \item{metadata}{List with name, description, source, version}
#'   \item{proficiency_levels}{Tibble defining the 0-4 proficiency scale}
#'   \item{nodes}{List of node tables:
#'     \describe{
#'       \item{competencies}{7 competency areas from IC CRG}
#'       \item{skills}{18 knowledge/skills (KSAs) curated for EDA4ML scope}
#'       \item{work_roles}{3 data science job roles}
#'       \item{courses}{6 courses aligned with textbook chapters}
#'       \item{learners}{6 fictional learner profiles}
#'     }
#'   }
#'   \item{edges}{List of edge tables:
#'     \describe{
#'       \item{has_skill}{Learner → Skill with proficiency weight (37 edges)}
#'       \item{requires_skill}{Work Role → Skill with required proficiency (25 edges)}
#'       \item{prerequisite}{Skill → Skill, directed learning dependencies (22 edges)}
#'       \item{teaches}{Course → Skill with level taught (21 edges)}
#'     }
#'   }
#' }
#'
#' @section Graph Structure:
#' The LearningGraph demonstrates several graph types:
#' \itemize{
#'   \item \strong{DAG}: The prerequisite edges form a directed acyclic graph
#'   \item \strong{Bipartite}: Learner-Skill and Course-Skill relationships
#'   \item \strong{Weighted}: Proficiency levels as edge weights
#'   \item \strong{Multi-relational}: Multiple edge types between node types
#' }
#'
#' @section Demonstrated Algorithms:
#' \itemize{
#'   \item \strong{Shortest path}: Find minimal learning sequence between skills
#'   \item \strong{Gap analysis}: Compare current vs. required skill profiles
#'   \item \strong{Community detection}: Skills cluster by competency area
#'   \item \strong{Centrality}: Identify foundational vs. advanced skills
#'   \item \strong{Topological sort}: Determine valid learning orderings
#' }
#'
#' @section Source:
#' Node and edge definitions are based on the \emph{Competency Resource Guide
#' for Data Science} (2023), published by the Office of the Director of

#' National Intelligence (ODNI). This document is UNCLASSIFIED and publicly
#' available.
#'
#' The knowledge graph structure is inspired by Workera.ai's skills intelligence
#' platform, which provides AI-powered skill assessment and personalized
#' learning paths for enterprise workforces.
#'
#' @section Example Usage:
#' \preformatted{
#' library(igraph)
#' library(dplyr)
#'
#' # Load the data
#' data(learning_graph)
#'
#' # Build prerequisite DAG
#' prereq_edges <- learning_graph$edges$prerequisite |>
#'   left_join(learning_graph$nodes$skills, by = c("skill_from_id" = "skill_id")) |>
#'   rename(from = skill_name) |>
#'   left_join(learning_graph$nodes$skills, by = c("skill_to_id" = "skill_id")) |>
#'   rename(to = skill_name) |>
#'   select(from, to)
#'
#' g <- graph_from_data_frame(prereq_edges, directed = TRUE)
#' plot(g, layout = layout_as_tree(g))
#'
#' # Find shortest learning path
#' shortest_paths(g, from = "Programming", to = "Statistical Learning")
#' }
#'
#' @seealso
#' \code{\link{lg_skills}} for the skills table alone,
#' \code{\link{lg_prerequisite}} for the prerequisite edge list
#'
#' @references
#' Office of the Director of National Intelligence. (2023).
#' \emph{Competency Resource Guide for Data Science}.
#' \url{https://www.dni.gov/}
#'
#' Workera.ai. (2024). \emph{Skills Intelligence Platform}.
#' \url{https://workera.ai/}
#'
#' @examples
#' # Summary of the knowledge graph
#' sapply(learning_graph$nodes, nrow)
#' sapply(learning_graph$edges, nrow)
#'
#' # View competencies
#' learning_graph$nodes$competencies
#'
#' # View proficiency levels
#' learning_graph$proficiency_levels
#'
#' @name learning_graph
#' @docType data
#' @keywords datasets graphs
NULL

#' @rdname learning_graph
#' @format Tibble with 5 rows: proficiency levels 0-4
"lg_proficiency_levels"

#' @rdname learning_graph
#' @format Tibble with 7 rows: data science competency areas
"lg_competencies"

#' @rdname learning_graph
#' @format Tibble with 18 rows: curated KSAs for EDA4ML scope
"lg_skills"

#' @rdname learning_graph
#' @format Tibble with 3 rows: data science work roles
"lg_work_roles"

#' @rdname learning_graph
#' @format Tibble with 6 rows: courses mapped to skills
"lg_courses"

#' @rdname learning_graph
#' @format Tibble with 6 rows: fictional learner profiles
"lg_learners"

#' @rdname learning_graph
#' @format Tibble with 37 rows: learner skill proficiencies
"lg_has_skill"

#' @rdname learning_graph
#' @format Tibble with 25 rows: role skill requirements
"lg_requires_skill"

#' @rdname learning_graph
#' @format Tibble with 22 rows: skill prerequisite edges (DAG)
"lg_prerequisite"

#' @rdname learning_graph
#' @format Tibble with 21 rows: course-skill teaching relationships
"lg_teaches"
