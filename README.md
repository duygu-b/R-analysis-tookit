# R-analysis-tookit
# R-Based Analysis Toolkit for Experimental Cognitive Science

This repository contains R scripts for power analysis, mixed-effects modeling, JSON data wrangling, and mouse-tracking analyses (AUC and Maximum Deviation) used in behavioral and cognitive experiments.

# bilingual_anova_power = Superpower Analysis
A power analysis using the **Superpower** package.

The analysis simulates a 3 × 2 × 2 mixed ANOVA with:
- Group: Turkish–English bilinguals, Spanish–English bilinguals, English monolinguals
- Sentence Type: Ambiguous, Unambiguous
- Stroop Condition: Congruent, Incongruent

Power was estimated using 1,000 simulations (α = .05, n = 45 per group).

# bilingual_lmer_model = Linear Mixed-Effects Analysis
A linear mixed-effects model using the **lme4** package.

The model examines the effects of:
- Previous Stroop condition
- Sentence type
- Language group

on log-transformed critical region reaction times.

Random intercepts are included for both participants and sentence items to account for individual and item-level variability.

# mouse_tracking_lme_analysis = Mouse Trajectory Analysis
Calculating mouse-tracking measures, including Maximum Deviation (MD) and Area Under the Curve (AUC), from normalized cursor trajectories.

The script:
- Normalizes trajectory time within each trial
- Computes MD and AUC as measures of movement dynamics
- Merges trial-level condition information
- Fits a linear mixed-effects model to examine the effect of previous Stroop condition on trajectory measures

Random intercepts are included for participants to account for individual variability.

# mouse_trajectory_preprocessing_unnest = Mouse Trajectory Data Processing for unnesting JSON file
Preprocessing mouse-tracking data stored in JSON format.

The script:
- Imports trial-level data from Excel
- Parses nested JSON mouse trajectories
- Converts trajectory coordinates into a long-format dataset
- Exports the processed data for subsequent mouse-tracking analyses (e.g., Maximum Deviation and Area Under the Curve)

# pilot_monolingual_lm = Pilot Linear Model (Monolingual Data)
A pilot analysis using a simple linear model in R.

The analysis:
- Loads monolingual pilot dataset
- Log-transforms reaction times
- Converts predictors into factors
- Tests main effects and interaction of sentence type and previous Stroop condition using a linear model (no random effects)

# unnesting_json_file = JSON ROI Transition Preprocessing
Preprocessing eye-tracking (or mouse-tracking) data containing JSON-encoded ROI transition information.

This script was developed using eye-tracking data and demonstrates a general workflow for converting nested JSON trajectory data into a structured long-format dataset.

The pipeline:
- Loads trial-level data from Excel
- Parses JSON strings containing ROI transition coordinates
- Expands nested trajectory data into long format (one row per timestamp)
- Filters data to retain relevant time windows (e.g., post-preparation phase)
- Exports cleaned dataset for further statistical analysis

Although this implementation uses eye-tracking data as an example, the same approach can be applied to any dataset containing JSON-encoded temporal or spatial trajectories.

