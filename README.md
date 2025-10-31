# 🛰️ SMNdataR
### Tools for Processing and Analyzing Mexican Meteorological Data for Climatological Applications

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.17451876.svg)](https://doi.org/10.5281/zenodo.17451876)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)
[![JOSS status](https://joss.theoj.org/papers/under_review.svg)](https://joss.theoj.org/)
[![R](https://img.shields.io/badge/R->=3.5-blue.svg)](https://cran.r-project.org/)
[![GitHub issues](https://img.shields.io/github/issues/sasoryhaf91/SMNdataR.svg)](https://github.com/sasoryhaf91/SMNdataR/issues)

---

## 📘 Overview

`SMNdataR` is an **open-source R package** designed to automate the **retrieval, cleaning, and analysis of climatological data** from Mexico’s *Servicio Meteorológico Nacional (SMN)*.  
It provides fully reproducible workflows for handling large volumes of daily meteorological data, integrating **station metadata**, **completeness checks**, and optional **hybrid fusion** with open global datasets such as **NASA POWER** and **CHIRPS**.

The package fills a major gap in the accessibility of meteorological information in Mexico, where open data are limited and existing systems (e.g., CLICOM, ERIC IV) do not allow for automated, large-scale downloads or integration into reproducible analytical pipelines.

---

## 🌎 Key Features

- 📥 **Automatic data retrieval:** Download daily SMN data for multiple stations at once.  
- 🧩 **Data harmonization:** Standardizes variable names, metadata, and coordinates.  
- 🌤️ **Hybrid integration:** Combines SMN data with global datasets (NASA POWER).  
- 🧮 **Analytical utilities:** Completeness, summary, and spatial correlation tools.  
- 📊 **Export-ready outputs:** Clean, tidy datasets compatible with modern R workflows (`tidyverse`, `sf`, etc.).  
- 🔁 **Reproducibility:** Fully scripted and transparent, following FAIR and open-science principles.  

---

## ⚙️ Installation

You can install the development version directly from GitHub:

```r
# Install dependencies (if necessary)
install.packages(c("devtools", "dplyr", "lubridate", "httr", "jsonlite", "sf"))

# Install SMNdataR from GitHub
devtools::install_github("sasoryhaf91/SMNdataR")

# Load the package
library(SMNdataR)
```

## 💡 Example Usage

```r
# Example: Retrieve SMN + NASA POWER (hybrid)
library(SMNdataR)

data <- smn_dl_daily_batch(
  stations   = c("15101", "15021"),
  source     = "hybrid",
  start_date = "1991-01-01",
  end_date   = "2020-12-31",
  vars       = c("T2M_MIN", "T2M_MAX", "PRECTOTCORR")
)

head(data)
```

This retrieves and merges daily precipitation and temperature records from SMN with satellite-derived 
variables from NASA POWER at the same coordinates.

## 📄 Citation

If you use `SMNdataR` in your research, please cite it as follows:

> Antonio-Fernández, H., Vaquera-Huerta, H., Rosengaus-Moshinsky, M. M., Pérez-Rodríguez, P., & Crossa, J. (2025).  
> *SMNdataR: Tools for Processing and Analyzing Mexican Meteorological Data for Climatological Applications.*  
> Journal of Open Source Software (submitted).
> DOI: [10.5281/zenodo.17451876](https://doi.org/10.5281/zenodo.17451876)

BibTeX entry:

```bibtex
@misc{Fernandez2025SMNdataR,
  author  = {Hugo Antonio-Fernández and Humberto Vaquera-Huerta and Moisés Michel Rosengaus-Moshinsky and Paulino Pérez-Rodríguez and José Crossa},
  title   = {SMNdataR: Tools for Processing and Analyzing Mexican Meteorological Data for Climatological Applications},
  year    = {2025},
  doi     = {10.5281/zenodo.17451876},
  url     = {https://github.com/sasoryhaf91/SMNdataR},
  publisher = {Zenodo},
}
```

## 🏛️ Funding and Acknowledgements

This work was supported by the *Consejo Nacional de Humanidades, Ciencias y Tecnologías* (CONAHCYT, formerly CONACYT–SECyT) through a doctoral scholarship granted to the first author under the National Postgraduate System of Mexico.  
We thank the *Servicio Meteorológico Nacional* (SMN) and the *Comisión Nacional del Agua* (CONAGUA) for providing open access to their climatological datasets.  
We also acknowledge the institutional collaboration and academic support from the *Colegio de Postgraduados*, the *Universidad Mexiquense del Bicentenario*, and *CIMMYT México*.  
Their contributions have been essential to developing reproducible tools that democratize access to meteorological data and promote open science in Mexico.

---

## 🧾 License

This package is released under the **MIT License**.  
See [LICENSE.md](LICENSE.md) for details.

---

## 🔗 Project Links

- 🧬 GitHub Repository: [https://github.com/sasoryhaf91/SMNdataR](https://github.com/sasoryhaf91/SMNdataR)  
- 🧾 Zenodo DOI: [https://doi.org/10.5281/zenodo.17451876](https://doi.org/10.5281/zenodo.17451876)  
- 📄 JOSS Submission: *(pending review)*  
- 🧠 Author ORCID: [0000-0002-5355-8476](https://orcid.org/0000-0002-5355-8476)

---

## 🤝 Contributing

Contributions, suggestions, and bug reports are welcome!  
Please open an [issue](https://github.com/sasoryhaf91/SMNdataR/issues) or submit a [pull request](https://github.com/sasoryhaf91/SMNdataR/pulls).  
If you wish to contribute new features, please describe your proposal clearly in the issue tracker before submitting a pull request.

---

## 🧠 Contact

**Hugo Antonio-Fernández**  
Doctoral Researcher — Colegio de Postgraduados / Researcher - Universidad Mexiquense del Bicentenario  
📧 Email: [uiem.haf@gmail.com](mailto:uiem.haf@gmail.com)  
🌐 ORCID: [0000-0002-5355-8476](https://orcid.org/0000-0002-5355-8476)

---

## 🧭 Citation Reminder

Please include the DOI and package name in any derivative work.  
Your citation helps recognize the effort invested in developing open and reproducible climatological tools for Mexico.

> Antonio-Fernández, H., Vaquera-Huerta, H., Rosengaus-Moshinsky, M. M., Pérez-Rodríguez, P., & Crossa, J. (2025).  
> *SMNdataR: Tools for Processing and Analyzing Mexican Meteorological Data for Climatological Applications.*  
> Journal of Open Source Software, *In Review*.  
> DOI: [10.5281/zenodo.17451876](https://doi.org/10.5281/zenodo.17451876)

  
