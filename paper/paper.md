---
title: "SMNdataR: Tools for Processing and Analyzing Mexican Meteorological Data for Climatological Applications"
tags:
  - R
  - meteorology
  - climatology
  - Mexico
  - reproducible research
authors:
  - name: "Hugo Antonio-Fernández"
    orcid: "0000-0002-5355-8476"
    affiliation: "1, 2"
  - name: "Humberto Vaquera-Huerta"
    orcid: "0000-0002-2805-804X"
    affiliation: 1
  - name: "Moisés Michel Rosengaus-Moshinsky"
    affiliation: 3
  - name: "Paulino Pérez-Rodríguez"
    orcid: "0000-0002-3202-1784"
    affiliation: 1
  - name: "José Crossa"
    orcid: "0000-0001-9429-5855"
    affiliation: 4
affiliations:
  - name: "Colegio de Postgraduados, México"
    index: 1
  - name: "Universidad Mexiquense del Bicentenario, México"
    index: 2
  - name: "Independent Consultant"
    index: 3
  - name: "CIMMYT, México"
    index: 4
date: "2025-10-26"
bibliography: paper.bib
repository: "https://github.com/sasoryhaf91/SMNdataR"
archive_doi: 10.5281/zenodo.17495178
---

# Summary

Access to high-quality and openly available meteorological data is essential for climate research, hydrological modeling, and environmental decision-making [@Hampton2015OpenScience; @Sandve2013TenSimpleRules]. In Mexico, however, access to meteorological information remains limited, fragmented, and largely manual. The *Servicio Meteorológico Nacional* (SMN) provides historical climate data through web interfaces and legacy systems such as CLICOM (Climate Computing Project) and ERIC (Extractor Rápido de Información Climatológica) [@CONAGUA2012SMN135; @Lobato2006ERICIII; @WMO1990CLICOM]. These systems, while important historically, lack programmatic access and do not support large-scale automated retrieval , limiting the development of reproducible and data-intensive climatological research.

`SMNdataR` was developed to overcome these barriers by providing an open-source, fully reproducible R package that automates the retrieval, cleaning, and analysis of meteorological data from all SMN stations. It offers a unified workflow that handles metadata inconsistencies, ensures data integrity, and integrates SMN data with open global sources such as NASA POWER [@NASA2020POWER]. Designed under the principles of open science and FAIR data (Findable, Accessible, Interoperable, Reusable) [@Wilkinson2016FAIR], `SMNdataR` allows researchers, policymakers, and educators to process millions of observations transparently and reproducibly. This contribution bridges a long-standing technological gap in Mexican climatology, facilitating national-scale analyses and supporting climate resilience, water management, and agricultural planning.

# Statement of need

Mexico’s complex geography spans tropical, temperate, arid, and high-altitude regions, resulting in one of the world’s most diverse climatic systems [@CONAGUA2012SMN135]. Understanding this variability requires continuous, high-resolution records. Despite a vast observational network maintained by the SMN, researchers face major barriers when attempting to access and process data reproducibly. 

Past efforts such as CLICOM and ERIC were designed for isolated station queries and manual downloads [@Lobato2006ERICIII; @WMO1990CLICOM]. They lack modern tools for automated data retrieval, quality control, or integration with analytical environments such as R or Python. Consequently, scientists studying climate, hydrology, and agriculture in Mexico have often relied on manual downloads or ad-hoc scripts, resulting in inconsistencies, duplication, and low reproducibility [@WMO2007GuideClimateData].

`SMNdataR` addresses these limitations by providing the first open-source, programmatic interface to SMN climate records, enabling large-scale automated access to daily data from thousands of stations. The package harmonizes metadata, corrects structural inconsistencies, and standardizes variable names and temporal formats following modern data-science conventions. It also integrates seamlessly with datasets such as NASA POWER, supporting comparative and cross-disciplinary analyses [@NASA2020POWER].

The main audience includes researchers in climatology, hydrology, and agronomy, as well as educators and policymakers interested in climate variability, drought monitoring, and water resource management. By automating data acquisition and promoting transparency, the package supports reproducible research and open data access in a national context where such tools have been historically absent [@Wilkinson2016FAIR]. Ultimately, `SMNdataR` helps democratize access to Mexico’s meteorological heritage and aligns national efforts with international open-science standards.

# Functionality and Implementation

`SMNdataR` is organized into modular function families that mirror a typical climatological workflow: **data download**, **metadata integration**, **data transformation**, and **quality evaluation**. Each family follows a consistent naming convention—`smn_dl_`, `smn_int_`, etc.—ensuring usability for both novice and advanced users.

The **data retrieval** functions automate access to SMN repositories through single-station (`smn_dl_daily_single`) and batch (`smn_dl_daily_batch`) routines. These handle connectivity, encoding, and date parsing automatically, producing tidy outputs ready for analysis.  
The **metadata** functions (`smn_int_extract_coordinates()`, `smn_int_get_station()`) harmonize identifiers, coordinates, and altitude information, allowing integration with spatial datasets or digital elevation models.

The package follows open-source best practices:

- **Reproducibility:** All functions return consistent outputs suitable for version control.  
- **Interoperability:** Fully compatible with the `tidyverse` ecosystem (`dplyr`, `ggplot2`, `sf`).  
- **Scalability:** Handles millions of daily records efficiently with vectorized operations.  
- **Extensibility:** Modular design allows future integration of ERA5, MODIS, or CHIRPS data.

A typical workflow retrieves daily precipitation and temperature data for several stations, merges them with NASA POWER variables, and exports a unified dataset:


```r
# Install development version
# remotes::install_github("sasoryhaf91/SMNdataR")
library(SMNdataR)

# Example: retrieve and merge SMN + NASA POWER data
data <- smn_dl_daily_batch(
  source = "hybrid",
  stations = c("15021", "15101"),
  start_date = "1991-01-01",
  end_date   = "2020-12-31",
  vars = c("PRECTOTCORR","EVLAND","T2M_MAX","T2M_MIN"),
  output_format = "full"
)
```

This workflow automates previously manual, error-prone tasks while maintaining complete traceability. The package thus represents a reproducible scientific infrastructure for modernizing climate data handling in Mexico.

# Related Work

Other R packages, including rnoaa [@Chamberlain2023], climateR [@Kemp2022], and nasapower [@Sparks2018], provide access to global datasets such as NOAA, NASA POWER, and WorldClim. However, none offer programmatic access to in situ data from Mexico’s SMN, which remain the most detailed historical records for the country. Legacy systems like CLICOM-CICESE [@CLICOM2022CICESE] and ERIC IV [@ERICIV2006] pioneered data availability but were designed for manual queries without interoperability for modern research.

SMNdataR uniquely focuses on harmonizing SMN’s heterogeneous records and integrating them with open global datasets, bridging the gap between traditional archives and modern climatological computing.

# Acknowledgements

This work was supported by the Secretaría de Ciencia, Humanidades, Tecnología e Innovación (SECIHTI) through a doctoral scholarship granted to the first author under Mexico’s National Postgraduate System. We acknowledge the Comisión Nacional del Agua (CONAGUA) for maintaining the national meteorological database forming the foundation of this package, and the Colegio de Postgraduados and Universidad Mexiquense del Bicentenario for institutional support. We also thank the International Maize and Wheat Improvement Center (CIMMYT) for fostering collaboration in open climate and agricultural research.

# References



