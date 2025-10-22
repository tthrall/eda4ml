#####
###
#     get_ref_materials.R
#       -- list reference materials via separate modules
###
#####

###
#   list_links()
#     -- reformat reference tibble as [title](link) by auth
###
list_links <- function(
    x,           # <tbl> tibble containing (title, link, auth)
    t_idx = 1L,  # <int> index of column containing title
    l_idx = 2L,  # <int> index of column containing link
    a_idx = NULL # <int> index of column containing author
) {
  cit_vec <- vector(mode = "character")
  t_vec   <- x |> dplyr::pull(var = t_idx)
  l_vec   <- x |> dplyr::pull(var = l_idx)

  # vector of citations w/o author
  for (r in 1:nrow(x)) {
    cit_vec [[r]] <- paste0(
      "[", t_vec [[r]], "](", l_vec [[r]], ")"
    )
  }

  # append author if available
  if (! is.null(a_idx)) {
    a_vec <- x |> dplyr::pull(var = a_idx)
    for (r in 1:nrow(x)) {
      cit_vec [[r]] <- paste0(
        cit_vec [[r]], " by ", a_vec [[r]]
      )
    }
  }
  return(cit_vec)
}

###
#   get_articles_tbl()
#     -- return a tibble of reference articles
###
get_articles_tbl <- function() {
  articles_tbl <- tibble::tribble(
    ~title, ~link,
    "CRAN Task View: Time Series Analysis", "https://cran.r-project.org/web/views/TimeSeries.html",
    "Fun with asta", "https://github.com/nickpoison/astsa/tree/master/fun_with_astsa"
  )
  return(articles_tbl)
}

###
#   get_books_tbl()
#     -- return a tibble of reference articles
###
get_books_tbl <- function() {
  books_tbl <- tibble::tibble(
    title = c(
      "Time Series (2e)",
      "Time Series Analysis and Its Applications: With R Examples (5e)"
    ),
    link = c(
      "https://doi.org/10.1137/1.9780898719246",
      "https://doi.org/10.1007/978-3-031-70584-7"
    ),
    auth = c(
      "David Brillinger",
      "Robert Shumway and David Stoffer"
    )
  )
  return(books_tbl)
}

###
#   get_symbol_tbl()
#     -- return a tibble of technical symbols
###
get_symbol_tbl <- function() {
  symbol_tbl <- tibble::tribble(
    ~tag, ~symbol, ~dscr,
	"B_space", "$\\mathcal{B}$",
	  "a Banach space (complete, normed, linear space of functions)",
	"B_star", "$\\mathcal{B}^*$",
	  "dual space of bounded linear functionals defined on $\\mathcal{B}$",
	"C_T", "$\\mathcal{C} (\\mathbb{T})$",
	  "Banach space of continuous functions defined on $\\mathbb{T}$",
	"C_m_T", "$\\mathcal{C}_m (\\mathbb{T})$",
	  "Subspace of functions having a continuous derivative of order $m$",
	"delta_t", "$\\delta_\\tau$",
	  "Dirac delta functional: $\\delta_\\tau (f) = f (\\tau)$",
	"D_m_T", "$\\mathcal{D}_m (\\mathbb{T})$",
	  "dual of $\\mathcal{C}_m (\\mathbb{T})$",
	"D_n", "$\\mathcal{D}_n (\\cdot)$",
	  "Dirichlet kernel function",
	"F", "$\\mathcal{F}$",
	  "mapping of a function or functional to its Fourier coefficients",
	"F_inv", "$\\mathcal{F}^{-1}$",
	  "mapping of Fourier coefficients to their function or functional",
	"f", "$f (\\theta)$",
	  "a periodic function of a real value",
	"f_nu", "$\\hat{f} [\\nu]$",
	  "Fourier coefficient of function $f$ of index $\\nu$",
	"phi", "$\\phi (\\omega)$",
	  "a function defined on the unit circle",
	"F_n", "$F_n (\\cdot)$",
	  "Fejér kernel function",
	"L_p", "$L_p (\\mathbb{T})$",
	  "functions whose p-th power is integrable",
	"l_p", "$\\mathcal{l}_p$",
	  "sequence of elements whose p-th power is summable",
	"lam", "$\\lambda (\\cdot)$",
	  "linear functional: linear, scalar-valued function defined on a linear space of functions",
	"lam_g", "$\\lambda_g (\\cdot)$",
	  "linear functional based on function g",
	"lam_mu", "$\\lambda_\\mu (\\cdot)$",
	  "linear functional based on measure $\\mu$",
	"lam_nu", "$\\hat{\\lambda} [\\nu]$",
	  "Fourier coefficient of linear functional $\\lambda$ of index $\\nu$",
	"lam_T", "$\\lambda_\\mathbb{T}$",
	  "Lebesgue measure on the unit circle scaled to yield unit total mass",
	"mu", "$\\mu (\\cdot)$",
	  "a complex-valued Borel measure",
	"mu_c", "$\\mu_c (\\cdot)$",
	  "the component of measure $\\mu$ that is absolutely continuous with respect to Lebesgue measure",
	"mu_d", "$\\mu_d (\\cdot)$",
	  "the discrete component of measure $\\mu$ concentrated on a countable set of atoms",
	"mu_abs", "$| \\mu | \\; (\\cdot)$",
	  "non-negative measure: modulus of measure $\\mu$",
	"M_T", "$\\mathcal{M} (\\mathbb{T})$",
	  "Banach space of totally finite Borel measures $\\mu (\\cdot)$",
	"omega", "$\\omega$",
	  "a complex number of modulus 1",
	"omega_n", "$\\omega_n$",
	  "n-th primitive root of unity, $e^{2 \\pi i / n}$",
	"Omega", "$\\Omega$",
	  "a square matrix having elements of the form $\\omega_n^{\\mu \\; \\nu}$",
	"p_q", "$(p, q)$",
	  "conjugate Hölder exponents (having harmonic mean 2)",
	"S_n", "$S_n (f)$",
	  "finite sum of Fourier terms approximating function $f (\\cdot)$",
	"S_n_w", "$S_{n, w} (f)$",
	  "finite weighted sum of Fourier terms approximating function $f (\\cdot)$",
	"t_theta", "$\\tilde{\\theta}$",
	  "$\\theta$ modulo $(- \\pi, \\pi]$",
	"T", "$\\mathbb{T}$",
	  "the unit circle: complex numbers of modulus 1",
	"u_nu", "$u_\\nu (\\cdot)$",
	  "$e^{i \\theta} \\mapsto e^{i \\nu \\theta}$, the unit exponential function of index $\\nu$",
	"norm", "$\\lVert \\cdot \\rVert$",
	  "norm of a function or linear functional",
	"norm_p", "${\\lVert \\phi \\rVert}_p$",
	  "the p-norm of a function",
	"inr_prd", "$\\left < \\phi, \\eta \\right >$",
	  "inner product of square-integrable functions"
  )
  return(symbol_tbl)
}

###
# EOF
###
