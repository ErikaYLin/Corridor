This repository contains development and testing code for corridor estimation methods and data integration to be implemented in the **"Continuous-Time Movement Modeling" (`ctmm`) `R` package**. To use the latest version of these methods, please visit the CTMM Initiative GitHub page and install the development version of the package. `ctmm` is also available for installation from CRAN using `install.packages("ctmm")`.

The `ctmm R` package and related materials can be found here: https://github.com/ctmm-initiative

----

**Scripts:**

* `corridor_ctmmlearn.R`: Example code for demonstrating the use of the cross-sectional KDE method for workshops.
* `corridor_debug.R`: Script for general debugging purposes for `corridor()`.
* `corridor_figures.R` (IN DEVELOPMENT): Generating generic figures for visualizing outputs with sample data for presentations. Figures related to specific case studies and/or manuscript preparation can be found in the scripts for those respective studies.
* `corridor_nonresident.R` (IN DEVELOPMENT): Testing code for non-resident movement models with corridor estimation.
* `corridor_sensitivity.R` (IN DEVELOPMENT): Comparative sensitivity analysis for sampling parameters across traditional occurrence distributions and cross-sectional KDE corridor distributions.
* `corridor_sensitivity.R`  (IN DEVELOPMENT): Improved sensitivity analysis to corridor traversal count, using a form of leave-one-out cross-validation.

