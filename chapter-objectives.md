# Chapter Learning Objectives

This document defines the learning objectives for each chapter of *EDA for Machine Learning*. These objectives serve as the specification for slide content and workbook exercises.

**Format**: Each objective uses an action verb implying a measurable outcome, following Bloom's taxonomy conventions.

---

## Part 1: Foundations of EDA

### Chapter 1: Exploratory Data Analysis

After completing this chapter, students will be able to:

1. Distinguish between exploratory and confirmatory data analysis and explain when each is appropriate.
2. Select appropriate measures of central tendency (mean, median) and dispersion (SD, IQR) based on data characteristics.
3. Construct basic visualizations: histograms, scatter diagrams, and box plots.
4. Interpret scatter plots, box plots, and histograms to identify patterns, outliers, and distributional shape.
5. Formulate questions that can initiate exploratory analysis of an unfamiliar dataset.

### Chapter 2: Conditional Distributions

After completing this chapter, students will be able to:

1. Define conditional expectation and the graph of averages. Explain their relevance to prediction.
2. Define z-score as the number of SDs above or below the mean. Convert a vector of numeric values to a vector of sample z-scores.
3. Distinguish between the SD line and the regression line and explain why the regression line is less steep.
4. Compute and interpret the correlation coefficient as a measure of linear association.
5. Explain why correlation does not imply causation and why zero correlation does not imply independence.
6. Recognize Simpson's paradox and identify when aggregated patterns may reverse upon disaggregation.
7. Apply the chi-squared test to assess independence of categorical variables.

### Chapter 3: Clustering: EDA in Higher Dimensions

After completing this chapter, students will be able to:

1. Explain why standardization (z-scores) is necessary before computing distances across variables with different scales.
2. Apply K-means clustering and interpret the resulting cluster assignments.
3. Use the elbow method to guide selection of the number of clusters.
4. Profile clusters by examining variable means and distributions within each group.
5. Evaluate clustering results using metrics such as within-cluster sum of squares and Jaccard similarity.
6. Distinguish between clustering for exploration versus clustering for comparison to a proposed classification.

### Chapter 4: Statistical Simulation

After completing this chapter, students will be able to:

1. For a given family of probability distributions, distinguish R's 4 types of function: d/p/q/r.
2. Generate a pseudo-random sample from common family of probability distributions.
3. Design and implement Monte Carlo simulations to estimate quantities of interest.
4. Compare estimators (e.g., mean vs. median) by examining their sampling distributions.
5. Apply the bootstrap to construct confidence intervals without relying on parametric formulas.
6. Explain when and why importance sampling is needed for rare event estimation.

### Chapter 5: Sampling and Study Design

After completing this chapter, students will be able to:

1. Explain why sample size alone does not guarantee valid inference.
2. Give an example of selection bias in an observational study and its consequences.
3. Distinguish between observational and experimental study designs.
4. Define a sampling frame. Explain how it affects generalizability to a target population.
5. Define and give examples of bias, chance error, and measurement uncertainty.

### Chapter 6: Information Theory

After completing this chapter, students will be able to:

1. Define, describe, and calculate entropy for finite probability distributions.
2. Define and describe mutual information.
3. Define and describe KL divergence.
4. Describe how entropy is related to decision tree splitting.
5. Give an example in which mutual information measures a nonlinear relationship that correlation does not measure.
6. Describe the relationship between cross-entropy and KL divergence.

---

## Part 2: Linear Algebra for Machine Learning

### Chapter 7: Linear Regression

After completing this chapter, students will be able to:

1. In a scatter diagram, identify whether a given point represents an observation or a feature.
2. In the standard formulation of a linear regression model, identify whether a column of the X matrix represents an observation or a feature.
3. Describe how the concept of orthogonal projection applies to linear least-squares regression.
4. In linear regression, define the normal equations.
5. In linear regression, define the orthogonal projection matrix derived from the normal equations.
6. Explain why centering data simplifies the geometry of linear regression.
7. In linear regression, explain why residuals are orthogonal to all feature vectors.
8. Describe covariance and correlation as inner products of centered vectors.

### Chapter 8: Principal Component Analysis

After completing this chapter, students will be able to:

1. Describe the goal and method of Principal Component Analysis (PCA).
2. Compute principal components via eigendecomposition of the covariance matrix.
3. Interpret loadings as the contribution of original variables to each principal component.
4. Use the scree plot to determine how many components to retain.
5. Project high-dimensional data onto principal components for visualization.
6. Explain when one should simply center the data, and when one should also re-scale the data, prior to PCA.

### Chapter 9: Linear Discriminant Analysis

After completing this chapter, students will be able to:

1. Explain the difference between LDA and PCA (maximizing variance versus maximizing class separation).
2. Explain Fisher's LDA criterion as the ratio of between-class to within-class variance.
3. Derive linear discriminant functions from multivariate normal class distributions.
4. Apply LDA for classification and interpret the resulting decision boundaries.
5. Explain how to determine when the equal-covariance assumption fails and QDA may be preferred.
6. Describe how discriminant directions are related to the eigenvalue problem involving within- and between-class covariance matrices.

---

## Part 3: Text Data

### Chapter 10: Text as Data

After completing this chapter, students will be able to:

1. Transform raw text into structured numerical representations suitable for analysis.
2. Apply standard preprocessing steps: tokenization, case normalization, stop word removal, and stemming.
3. Construct and interpret document-term matrices.
4. Calculate TF-IDF weights and explain what they capture that raw counts do not.
5. Explain how structured text representations enable machine learning on unstructured data.

### Chapter 11: Topic Models

After completing this chapter, students will be able to:

1. Describe the generative model underlying Latent Dirichlet Allocation (LDA).
2. Interpret word mixtures per topic (β) and topic mixtures per document (γ).
3. Fit a topic model to a corpus of text documents.
4. Qualitatively evaluate a fitted topic model using the criteria of coherence and interpretability.
5. Select the number of topics using: (1) held-out likelihood; and (2) domain judgment.
6. Describe Latent Dirichlet Allocation, Linear Discriminant Analysis, and the difference between the two.
7. Apply topic models to discover thematic structure in document collections.

---

## Part 4: Time Series Data

### Chapter 12: Time Series Data

After completing this chapter, students will be able to:

1. Describe the difference between time domain and frequency domain perspectives on time series.
2. Explain why time series data requires specialized methods (dependence structure, autocorrelation).
3. Decompose a time series visually into trend, seasonality, and residual components.
4. Assess whether a time series is approximately stationary.
5. In time series analysis, define and explain the role of the time-shift operator.

### Chapter 13: Time Domain Methods

After completing this chapter, students will be able to:

1. Identify AR, MA, and ARMA processes from their ACF and PACF signatures.
2. Fit ARIMA models and interpret their parameters.
3. Use differencing to remove a trend from a time series.
4. Generate forecasts with prediction intervals and explain why intervals widen over time.
5. Diagnose model adequacy by checking that residuals approximate white noise.
6. Compare models using information criteria AIC and BIC.

### Chapter 14: Frequency Domain Methods

After completing this chapter, students will be able to:

1. Interpret the spectrum as the decomposition of variance across frequencies.
2. Estimate the spectrum density function using the periodogram.
3. Explain why the periodogram must be smoothed to obtain a consistent estimator of the spectrum density function.
4. Identify periodic components from spectral peaks and convert frequency to period.
5. Explain the bias-variance tradeoff in spectral estimation (bandwidth selection).
6. Apply coherence analysis to assess the relationship between two time series across frequencies.
7. Describe the spectrum density function of an AR(1) or MA(1) model.

---

## Part 5: Graph Data

### Chapter 15: Graph Theory for Machine Learning

After completing this chapter, students will be able to:

1. Represent relational data as a graph composed of nodes and edges.
2. Compute and interpret centrality measures: degree, betweenness, and PageRank.
3. Apply community detection algorithms and interpret modularity scores.
4. Project bipartite graphs onto single-mode networks and interpret the resulting structure.
5. Extract graph-derived features for use in downstream machine learning models.
6. Describe differences between graph analysis for understanding (what structure exists?) versus prediction (what will happen?).

---

## TODOs

| Chapter | Topic | Note |
|---------|-------|------|
| 1 | Exploratory vs. confirmatory | Bolster content with contrast: rationale and historical development |
| 2 | SD line vs. regression line | Make foreshadowing of loss functions more explicit |
| 3 | Clustering as comparison | Add example comparing algorithmic clustering to a proposed classification scheme |
| 5 | Sampling frame | Give an example of a sampling frame in a social survey |
| 5 | Measurement error | Define the standard model of measurement error |
| 9 | Exercises | Consider adding one or more exercises |
| 11 | Coherence | Consider adding more on coherence as a measure |

---

## Revision History

- 2025-12-22: Complete draft (all 15 chapters); initial review of Chapters 1–3
- 2025-12-22: Batch review and refinement (all chapters); TODOs consolidated
