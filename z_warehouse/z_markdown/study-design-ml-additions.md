# Suggested ML Additions for `study-design.qmd`

## 1. Grammar Fix (line 84)

**Current text:**
```
- How and how well is the performance of the deployed model be monitored?
```

**Revised:**
```
- How and how well will the performance of the deployed model be monitored?
```

---

## 2. MLOps Introduction (insert after line 87)

Add this paragraph after the bulleted list:

```markdown
These concerns are central to *machine learning operations* (MLOps), an emerging discipline that extends DevOps principles to the full model lifecycle [@IBM-MLOps-2025]. MLOps emphasizes that model performance depends not only on algorithm selection, but on the entire pipeline from data acquisition through deployment and monitoring. The historical examples in this chapter illustrate how foundational issues in study design—issues that predate ML by decades—remain decisive for modern algorithmic systems.
```

---

## 3. ML Callbacks by Section

### 3a. After Literary Digest section (insert after line 146, before "### Truman versus Dewey")

```markdown
::: {.callout-note}
## ML Connection
The *Digest* poll illustrates *selection bias*: the sample systematically differed from the population in ways that affected the outcome of interest. In ML terms, the training data did not represent the deployment environment. A model trained on *Digest* respondents would have learned patterns (e.g., political preferences of car and telephone owners) that fail to generalize to the broader electorate.
:::
```

### 3b. After quota sampling discussion (insert after line 181, before "### Using Chance in Surveys")

```markdown
::: {.callout-note}
## ML Connection
Quota sampling attempted to match the sample to the population on observable features (gender, employment status, etc.), but missed latent factors that influenced voting. This is analogous to stratified sampling on measured covariates while ignoring unmeasured confounders. In ML practice, ensuring that training data covers the relevant *feature space* is necessary but not sufficient; the joint distribution of features and labels must also match deployment conditions.
:::
```

### 3c. After Salk vaccine discussion (insert after line 341, before "### Portacaval Shunt")

```markdown
::: {.callout-note}
## ML Connection
The double-blind protocol ensured that neither the subjects nor the evaluators knew treatment assignments—eliminating a source of systematic bias in the labels (polio diagnosis). In supervised ML, analogous rigor is needed in annotation protocols: annotators should not have access to information that could bias their labels. The NFIP's initial design, where consent status was confounded with treatment, mirrors *label leakage*—a situation where the label-generation process is contaminated by information that would not be available at prediction time.
:::
```

### 3d. After Portacaval shunt discussion (insert after line 415, before "### Repeated Weighing of NB10")

```markdown
::: {.callout-note}
## ML Connection
The portacaval studies demonstrate how non-randomized comparisons can produce misleading results. In ML terms, this is analogous to evaluating a model on a test set that is not exchangeable with the training set—for instance, when healthier patients are systematically routed to treatment (or to training data) while sicker patients end up in controls (or the test set). The resulting performance metrics overstate the model's true effectiveness. Randomized train/test splits, stratified appropriately, guard against this bias.
:::
```

### 3e. After NB10 key points (insert after line 636, before "## Accuracy of the Sample Average")

```markdown
::: {.callout-note}
## ML Connection
The NB10 measurements illustrate two issues that carry directly into ML:

1. **Systematic bias**: The weight's true value differs from its nominal value by a consistent amount (~405 µg). In ML, analogous biases arise when labels are systematically shifted from ground truth—for example, when human annotators consistently over- or under-estimate a quantity.

2. **Non-normal errors**: Even under highly controlled conditions, the error distribution has heavier tails than the normal distribution (outliers at indices #86 and #94). ML systems trained on data with such outliers may be unduly influenced by them unless robust methods are employed.

These measurement-level concerns propagate through any downstream model. EDA's role is to detect such issues before they compromise model training.
:::
```

---

## 4. Optional: Section on EDA as the Bridge (new subsection)

If you want a more explicit bridge section, consider adding this before the Team Exercises:

```markdown
## The Role of EDA

Exploratory data analysis occupies a critical position between study design and model development. The study design determines what data *could* be collected and how representative it *should* be; EDA reveals what was *actually* collected and whether the design intentions were achieved.

For ML practitioners, EDA answers questions such as:

- Does the feature distribution in the training data match the expected deployment distribution?
- Are there systematic patterns in missing data that could bias the model?
- Do the labels exhibit the reliability implied by the labeling protocol?
- Are there outliers or anomalies that warrant investigation before model fitting?

The examples in this chapter—from the *Literary Digest* poll to the NB10 measurements—show that study design flaws are often invisible until the data are examined. No algorithm can overcome a fundamentally flawed data collection process; EDA is the diagnostic step that reveals whether the data support the intended analysis.
```

---

## Summary of Insertions

| Location | Content |
|----------|---------|
| Line 84 | Grammar fix |
| After line 87 | MLOps paragraph |
| After line 146 | Literary Digest callout |
| After line 181 | Quota sampling callout |
| After line 341 | Salk vaccine callout |
| After line 415 | Portacaval callout |
| After line 636 | NB10 callout |
| Before Team Exercises (optional) | EDA bridge section |
