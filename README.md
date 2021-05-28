## Language profile of young children with 22q11DS (3T project)
This project looks at the language skills of young children with the 22q11.2 deletion syndrome
This repository contains the code used for analysis in paper X
This code is specifically made for the analysis of specific data
The original data reported on in paper X cannot be shared, but there is a fake data file (randomly generated taking into account the context of the study)
Pull via SSH: `git clone git@github.com:3T-Emma/Testrepo3T.git`
The R markdown file provides further explanation

## specs 
R version 4.0.2 (2020-06-22)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 8.1 x64 (build 9600)

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] effectsize_0.4.4-1 broom_0.7.1        ppcor_1.1          MASS_7.3-51.6      expss_0.10.6       pastecs_1.3.21     forcats_0.5.0      stringr_1.4.0     
 [9] purrr_0.3.4        readr_1.4.0        tibble_3.0.3       tidyverse_1.3.0    e1071_1.7-3        rstatix_0.6.0      ggpubr_0.4.0       tidyr_1.1.2       
[17] dplyr_1.0.2        ggplot2_3.3.2      readxl_1.3.1      


## Project organization
- PG = project-generated
- HW = human-writable
- RO = read only
```
.
├── .gitignore
├── README.md
├── bin                <- Compiled and external code, ignored by git (PG)
│   └── external       <- Any external source code, ignored by git (RO)
├── config             <- Configuration files (HW)
├── data               <- All project data, ignored by git
│   ├── processed      <- The final, canonical data sets for modeling. (PG)
│   ├── raw            <- The original, immutable data dump. (RO)
│   └── temp           <- Intermediate data that has been transformed. (PG)
├── docs               <- Documentation notebook for users (HW)
│   ├── manuscript     <- Manuscript source, e.g., LaTeX, Markdown, etc. (HW)
│   └── reports        <- Other project reports and notebooks (e.g. Jupyter, .Rmd) (HW)
├── results
│   ├── figures        <- Figures for the manuscript or reports (PG)
│   └── output         <- Other output for the manuscript or reports (PG)
└── src                <- Source code for this project (HW)

```


## License

This project is licensed under the terms of X
