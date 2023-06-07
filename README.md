# Description

This repository contains data and code associated with the following manuscript (submitted):

Economou, M., Vanden Bempt, F., Van Herck, S., Wouters J.,  Ghesquière, P., Vanderauwera, J. and Vandermosten, M. Myelin plasticity during early literacy training in at-risk pre-readers.

Content:

- The script 01_load_data.R loads (and/or installs if needed) the required packages, defines custom helper functions and loads the dataset. 
- The script 02_analysis.R performs the main analysis for the manuscript. Specifically, it fits linear (mixed-effects) models for risk-related and intervention-related comparisons and also calculates effect sizes for both baseline, as well as pre-post comparisons. This script also saves all generated output, which can further be used to extract statistics and create figures.
- The script 03_small_data.R loads raw data, as well as model output and extracts the statistics reported in the manuscript in the Results section.
- The script 04_tables_figures.R loads raw data and model output and provides the necessary commands to reproduce the tables and figures of the manuscript.

Notes:
- Every script should be standalone, such that if one wants to only generate the figures, they could directly run script #04, which will then execute every necessary previous step to generate the figures and tables.


Shield: [![CC BY 4.0][cc-by-shield]][cc-by]

This work is licensed under a
[Creative Commons Attribution 4.0 International License][cc-by].

[![CC BY 4.0][cc-by-image]][cc-by]

[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-image]: https://i.creativecommons.org/l/by/4.0/88x31.png
[cc-by-shield]: https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg

This license allows reusers to distribute, remix, adapt, and build upon the material in any medium or format, so long as attribution is given to the creator.
