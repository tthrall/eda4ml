# EDA for Machine Learning — Book Positioning

## One-Sentence Positioning

*EDA for Machine Learning* bridges the gap between data wrangling (R4DS) and statistical learning (ISLR2), providing the conceptual and mathematical foundations that make ML methods interpretable rather than opaque.

---

## Target Reader Profile

A quantitatively-minded student who enjoys mathematics and wants to understand *why* methods work before applying them. They learn best by seeing the big picture first—geometric intuition, conceptual frameworks—then filling in the details through practice. They're not afraid of notation, but they also want to build practical skills. They've learned enough R to be functional (perhaps via R4DS or a first course) and are preparing for serious ML coursework (ISLR2, ESL, or equivalent). They find purely cookbook approaches unsatisfying but aren't ready for measure-theoretic foundations.

---

## Who This Book Is For (draft preface text)

This book is for students preparing for advanced machine learning coursework who want more than recipes. If you've learned to wrangle data in R and are ready to study classification, regression, and clustering in depth, this book provides the foundations you'll need: linear algebra as geometry, dimension reduction as projection, time series as both forecasting tool and window into temporal structure, graphs as relational data, and information theory as the language of uncertainty.

The emphasis throughout is on *understanding*—seeing why OLS is orthogonal projection, why PCA finds directions of maximum variance, why the spectrum and ACF are Fourier pairs. This geometric and conceptual grounding makes the methods you'll encounter in ISLR2 and beyond feel less like arbitrary procedures and more like natural consequences of the underlying mathematics.

The book assumes comfort with mathematical notation and a willingness to engage with derivations, but not prior exposure to linear algebra or statistics beyond a first course. Code is integrated throughout, but the goal is insight, not just implementation.

---

## Comparative Positioning

| Dimension | R4DS | ISLR2 | This Book |
|-----------|------|-------|-----------|
| **Primary audience** | Practitioners learning R/tidyverse | Students learning ML theory | Students preparing for ML coursework |
| **Assumed background** | Minimal | Some statistics, linear algebra | Comfortable with math, learning R |
| **Core emphasis** | Workflow: wrangle, visualize, communicate | Methods: regression, classification, clustering | Foundations: why methods work, geometric intuition |
| **Relationship to data** | Data is messy; learn to clean it | Data is given; learn to model it | Data is informative; learn to explore it |
| **Math stance** | Avoid it | Present it accessibly | Embrace it as source of insight |
| **Code stance** | Central; learn by typing | Labs supplement concepts | Integrated; `pkg::fn()` throughout |
| **EDA treatment** | Chapter 1, then move on | Brief mention | The entire book |

**Sequence:** R4DS → **This Book** → ISLR2

---

## Unique Value

Topics and perspectives not well-covered by R4DS or ISLR2:

- **Dual-aims framing**: Understanding vs. prediction as complementary goals throughout
- **Study design**: FPP-style treatment of observational vs. experimental data, sampling, and confounding
- **Information theory**: Entropy, mutual information, KL divergence as foundations for ML loss functions
- **Time series depth**: Three chapters covering data characteristics, time domain methods, and frequency domain methods
- **Graph theory for ML**: Centrality measures, community detection, graph-derived features, bipartite projections
- **Geometric intuition**: Regression as orthogonal projection, PCA as variance maximization, LDA as supervised projection

---

## Informal Framing

"It's for the student who actually *likes* math and learns by first seeing the big picture, but also wants to learn a useful craft."
