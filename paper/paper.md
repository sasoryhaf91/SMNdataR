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
  - name: "Colegio de Postgraduados, Mexico"
    index: 1
  - name: "Universidad Mexiquense del Bicentenario, Mexico"
    index: 2
  - name: "Independent Consultant"
    index: 3
  - name: "CIMMYT, Mexico"
    index: 4
date: "2025-10-26"
bibliography: paper.bib
repository: "https://github.com/sasoryhaf91/SMNdataR"
archive_doi: 10.5281/zenodo.17495178
---

# Summary

Access to open and high-quality meteorological data is essential for climate research, hydrological modeling, 
and environmental decision-making [@Hampton2015OpenScience; @Sandve2013TenSimpleRules]. However, in Mexico, 
access to meteorological information remains limited, fragmented, and largely manual. 
Although the SMN provides historical climate data through 
web-based interfaces and legacy systems such as CLICOM (Climate Computing Project) and 
ERIC (Extractor Rápido de Información Climatológica), these platforms lack programmatic access 
and do not support large-scale automated retrieval [@CONAGUA2012SMN135;@Lobato2006ERICIII;@WMO1990CLICOM]. 
This has hindered the development of reproducible and data-intensive climatological studies in the country.

`SMNdataR` was developed to overcome these barriers by offering an open-source, 
fully reproducible R package that automates the retrieval, cleaning, and analysis of daily meteorological records from all SMN stations. The package provides a unified framework for 
handling metadata inconsistencies, ensuring data integrity, and integrating SMN data with global 
open sources such as NASA POWER [@NASA2020POWER].

Designed under the principles of open science and FAIR data (Findable, Accessible, Interoperable, Reusable)
[@Wilkinson2016FAIR], `SMNdataR` enables researchers, policymakers, 
and educators to access and process millions of observations through a transparent and replicable workflow. 
This contribution bridges a long-standing technological gap in Mexican climatology, facilitating national-scale 
analyses and supporting climate resilience, water management, and agricultural planning initiatives.



# Statement of need

Mexico has one of the most diverse climatic and geographic systems in the world, spanning tropical, 
temperate, arid, and high-altitude regions. Understanding this variability requires continuous, 
high-resolution meteorological records [@CONAGUA2012SMN135]. Despite the existence of a vast observational network maintained 
by the SMN, researchers and institutions face major barriers when attempting 
to access, process, and analyze these data in a reproducible way.

Historically, efforts such as CLICOM and ERIC were designed to store and visualize meteorological records 
[@Lobato2006ERICIII;@WMO1990CLICOM].
While they represented important milestones for data digitization in the 1990s and 2000s, 
these systems were conceived for isolated station-based queries and manual downloads. 
They lack modern functionalities for automated data retrieval, quality control, or integration with 
analytical environments such as R or Python. As a result, scientists working on climate, 
hydrology, and agricultural modeling in Mexico have traditionally been forced to perform manual 
downloads or develop ad-hoc scripts, leading to inconsistencies, data duplication, and limited 
reproducibility [@WMO2007GuideClimateData].

`SMNdataR` directly addresses this long-standing limitation by providing 
the first open-source, programmatic interface to SMN climate records 
allowing large-scale, automated access to daily and monthly meteorological 
data for thousands of stations. 
The package introduces a unified data model that harmonizes station metadata, 
corrects encoding and structural inconsistencies, and standardizes variable names 
and temporal formats according to modern data-science conventions. This design facilitates 
integration with global datasets such as NASA POWER, 
expanding the potential for comparative, regional, and cross-disciplinary analyses [@NASA2020POWER].

The main audience for `SMNdataR` includes researchers in climatology, hydrology, 
and agronomy, as well as educators and policymakers interested in understanding climate variability, 
drought monitoring, and water resource management. By automating data acquisition and ensuring 
transparency in data processing, the package supports the principles of reproducible research 
and promotes open data access in a national context where such tools have been historically absent [@Wilkinson2016FAIR].

Ultimately, `SMNdataR` is not only a technical contribution but also a step toward democratizing access 
to Mexico’s meteorological heritage, enabling researchers to focus on scientific inquiry rather than data wrangling. 
It bridges the gap between traditional archives and modern computational climatology, helping position Mexico within 
the global movement toward open, interoperable, and sustainable environmental data infrastructures. 

This package represents the first reproducible open-source framework for accessing and standardizing SMN data, 
aligning national efforts with international standards for open climate information


# Functionality and Implementation

`SMNdataR` was designed as a modular and extensible framework that facilitates reproducible access, 
cleaning, and analysis of meteorological records from Mexico’s SMN. 
The package architecture is organized into logical families of functions that mirror the stages of a typical 
climatological workflow: data download, metadata integration, data transformation, and quality evaluation. 
Each family follows a consistent naming convention—`smn_dl_`, and `smn_int_`,—ensuring 
transparency and usability for both novice and advanced users.

### Data retrieval (`smn_dl_`)

The `smn_dl_` family automates the download of daily records from the official SMN repositories. 
It supports station-level queries (`smn_dl_daily_single`) and large-scale batch retrieval (`smn_dl_daily_batch`), 
allowing researchers to access thousands of stations with minimal configuration. 
These functions handle connectivity, encoding inconsistencies, and date parsing automatically, 
ensuring that all outputs are standardized as tidy data frames ready for analysis in R. 
Users can specify variables of interest (e.g., precipitation, maximum/minimum temperature, evaporation) 
and time ranges without manual interaction with SMN’s web interface.

### Metadata extraction and integration (`smn_int_`)

The `smn_int_` family is responsible for harmonizing and validating station metadata.
Functions such as `smn_int_extract_coordinates()` and `smn_int_get_station()` 
correct inconsistencies in station identifiers, latitude/longitude formats, and altitude values. 
The metadata layer is essential for spatial applications, enabling integration with digital elevation models or 
hydrological basins. Additionally, `SMNdataR` incorporates an internal metadata registry that cross-references 
official SMN identifiers with those used in legacy systems (CLICOM/ERIC), ensuring backward compatibility and traceability.

### Design principles

The development of `SMNdataR` followed open-source best practices:  
- Reproducibility: All functions return transparent and consistent outputs suitable 
for version control and reproducible pipelines.  
- Interoperability: The package adheres to the tidyverse data structure, 
ensuring compatibility with popular analytical frameworks such as `dplyr`, `ggplot2`, and `sf`.  
- Scalability: Efficient handling of large datasets (millions of daily records) was prioritized 
by using vectorized operations and memory-safe download routines.  
- Extensibility: Modular design allows future inclusion of additional sources, 
such as ERA5 or MODIS-based variables, with minimal changes.

### Example workflow

A typical use case involves retrieving daily rainfall, evaporation, and temperature data for multiple SMN stations, 
merging them with NASA POWER variables, and producing a complete time series for climatological analysis:

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
This workflow illustrates the capacity of SMNdataR to automate tasks that previously 
required extensive manual effort, while maintaining complete traceability and reproducibility of 
the data processing chain. The package is therefore not only a technical tool but a scientific 
infrastructure that supports the modernization of climate data handling in Mexico.

# Related Work

Several open-source tools have been developed to facilitate access to meteorological and 
climatological datasets worldwide. Packages such as `rnoaa` [@Chamberlain2023], `climateR`
[@Kemp2022], and `nasapower` [@Sparks2018] provide programmatic interfaces to global 
repositories including NOAA, NASA POWER, and WorldClim. These initiatives have significantly advanced 
the accessibility and reproducibility of climate research data, particularly for regions where such 
data arex openly distributed. However, none of these packages provide access to in situ observations 
from Mexico’s SMN, which remain the primary and most detailed source  of historical climate data in the country.

In Mexico, several legacy systems have attempted to make SMN data more accessible, including CLICOM-CICESE [@CLICOM2022CICESE] 
and ERIC IV [@ERICIV2006]. While these platforms were pioneering efforts for their time, they were designed for 
manual queries and individual station downloads, offering no programmatic or large-scale access capabilities. 
Their closed architectures and lack of interoperability with modern analytical environments limit their usefulness 
for contemporary data-intensive research.

`SMNdataR` represents the first open-source software designed specifically for programmatic access, harmonization, 
and analysis of SMN meteorological data. Unlike general-purpose global packages, `SMNdataR` directly addresses the 
structural and semantic heterogeneity of Mexican climate records—correcting inconsistencies in station metadata, 
variable naming, and temporal formats. Moreover, it integrates seamlessly with open global databases such 
as NASA POWER, enabling users to construct hybrid datasets that combine ground-based and satellite-derived 
information within a single, reproducible workflow.

Beyond technical accessibility, `SMNdataR` contributes to the principles of open science in Mexico, 
a context where official meteorological data have historically been difficult to access in standardized digital form. 
By providing a transparent and automated interface to the SMN archive, the package bridges the gap between local 
institutional data repositories and global scientific infrastructures. This contribution not only supports national 
research initiatives on climate, water, and agriculture but also positions Mexico within the international movement 
toward open, interoperable, and FAIR-compliant environmental data systems.

# Acknowledgements

This work was supported by the Secretaría de Ciencia, Humanidades, Tecnología e Innvoción
(SECIHTI) through a doctoral scholarship granted to the first author 
under the National Postgraduate System of Mexico. We acknowledge the Comisión Nacional del Agua (CONAGUA) 
for maintaining and providing access to the national meteorological 
database that forms the foundation of this package.

We also thank the Colegio de Postgraduados and the Universidad Mexiquense del Bicentenario for institutional 
support during the development of this research, 
as well as the International Maize and Wheat Improvement Center (CIMMYT) for fostering open 
collaboration in climate and agricultural research. Their combined contributions have been essential to 
the conception, validation, and dissemination of the `SMNdataR` package.


# References



