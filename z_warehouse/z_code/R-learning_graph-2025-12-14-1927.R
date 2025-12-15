#' LearningGraph: A Knowledge Graph for Skills-Based Learning
#'
#' A multi-relational knowledge graph representing the data science skills
#' ecosystem, designed for demonstrating graph theory concepts in machine
#' learning applications.
#'
#' @format A list with the following components:
#' \describe{
#'   \item{metadata}{List with name, description, source, version, schema}
#'   \item{proficiency_levels}{Tibble defining the 0-4 proficiency scale}
#'   \item{nodes}{List of node tables:
#'     \describe{
#'       \item{competencies}{7 competency areas from IC CRG}
#'       \item{skills}{18 knowledge/skills (KSAs) curated for EDA4ML scope}
#'       \item{work_roles}{3 data science job roles}
#'       \item{courses}{15 courses covering all 18 skills}
#'       \item{learners}{6 fictional learner profiles}
#'     }
#'   }
#'   \item{edges}{List of edge tables:
#'     \describe{
#'       \item{has_skill}{Learner -> Skill with proficiency (37 edges)}
#'       \item{requires_skill}{Work Role -> Skill with threshold (25 edges)}
#'       \item{prerequisite}{Skill -> Skill, conceptual dependencies (22 edges)}
#'       \item{course_prereq}{Course -> Course, curricular ordering (19 edges)}
#'       \item{teaches}{Course -> Skill with level taught (19 edges)}
#'     }
#'   }
#' }
#'
#' Individual components are also exported: \code{lg_proficiency_levels},
#' \code{lg_competencies}, \code{lg_skills}, \code{lg_work_roles},
#' \code{lg_courses}, \code{lg_learners}, \code{lg_has_skill},
#' \code{lg_requires_skill}, \code{lg_prerequisite}, \code{lg_course_prereq},
#' \code{lg_teaches}, \code{lg_schema}.
#'
#' @section Graph Structure:
#' \itemize{
#'   \item \strong{DAG}: Skill and course prerequisites form directed acyclic graphs
#'   \item \strong{Bipartite}: Learner-Skill and Course-Skill relationships
#'   \item \strong{Weighted}: Proficiency levels as edge weights
#'   \item \strong{Multi-relational}: Multiple edge types between node types
#' }
#'
#' @section Proficiency Framework:
#' Work role requirements follow typical career progression:
#' \itemize{
#'   \item Entry/Mid (2-5 yrs): concentration skills at level 2, supporting at 1-2
#'   \item Mid (3-5 yrs): concentration skills at 2-3, supporting at 2
#'   \item Senior (5+ yrs): concentration skills at 3, supporting at 2
#' }
#' Level 4 (Master) is reserved for thought leaders, not job requirements.
#'
#' @section Course Structure:
#' The 15 courses cover all 18 skills. Entry points (no prerequisites):
#' C01 (Programming), C03 (Probability), C04 (Linear Algebra).
#' Course design inspired by Georgia Tech OMSA, UC Berkeley MIDS,
#' Stanford MS Statistics, CMU MSML, and JHU MS Data Science.
#'
#' @section Complementary Prerequisite Views:
#' \itemize{
#'   \item \strong{prerequisite} (skill -> skill): Conceptual dependencies
#'   \item \strong{course_prereq} (course -> course): Curricular ordering
#' }
#' These are complementary; courses bundle skills, so course prerequisites
#' don't derive directly from skill prerequisites.
#'
#' @section Source:
#' Based on the \emph{Competency Resource Guide for Data Science} (2023),
#' Office of the Director of National Intelligence (UNCLASSIFIED).
#' Structure inspired by Workera.ai's skills intelligence platform.
#'
#' @examples
#' # Structure summary
#' sapply(learning_graph$nodes, nrow)
#' sapply(learning_graph$edges, nrow)
#'
#' # View courses and prerequisites
#' learning_graph$nodes$courses
#' learning_graph$edges$course_prereq
#'
#' @references
#' Office of the Director of National Intelligence. (2023).
#' \emph{Competency Resource Guide for Data Science}.
#' \url{https://www.dni.gov/}
#'
#' Workera.ai. (2024). \emph{Skills Intelligence Platform}.
#' \url{https://workera.ai/}
#'
#' @seealso \code{\link{lg_skills}}, \code{\link{lg_courses}},
#'   \code{\link{lg_prerequisite}}, \code{\link{lg_course_prereq}}
#'
#' @name learning_graph
#' @docType data
#' @keywords datasets graphs
NULL

#' @rdname learning_graph
"lg_proficiency_levels"

#' @rdname learning_graph
"lg_competencies"

#' @rdname learning_graph
"lg_skills"

#' @rdname learning_graph
"lg_work_roles"

#' @rdname learning_graph
"lg_courses"

#' @rdname learning_graph
"lg_learners"

#' @rdname learning_graph
"lg_has_skill"

#' @rdname learning_graph
"lg_requires_skill"

#' @rdname learning_graph
"lg_prerequisite"

#' @rdname learning_graph
"lg_course_prereq"

#' @rdname learning_graph
"lg_teaches"

#' @rdname learning_graph
"lg_schema"
