# README

### Replication files for "Rally 'round the barrack: Far-right support and the military", by Francisco Villamil, Stuart J. Turnbull-Dugarte, and José Rama.

This folder includes all the necessary materials to replicate the results from both the main Article and the Online Appendix. Most of them have been run using R 3.6 on macOS 12.3. We include the output from `sessionInfo()` at the end of this readme file. **NOTE:** The spatial analyses are computationally expensive and need a lot of RAM. We run it on a Google Cloud VM instance (e2-highmem-16, 128 GB RAM), and took around 72h. Without the spatial analyses, the code just takes a few minutes to build.

The code is organized in tasks that depend on the results of the previous ones. All the code is self-contained. Each folder contains the code and the output of one task. The taskflow is as follows:

![taskflow](taskflow/workflow.jpeg)

In addition, there is an additional task (`cses`) that is self-contained.

---

#### Raw data files

There are two types of raw files that are needed to run the analyses. The `download_shp` contains shapefiles for Spanish provinces and census sections, while the `input_data` folder contains a set of files that were previously downloaded or created:

- `barometers_full.csv`: individual-level dataset that contains all surveys (monthly barometers) merged, which is used in the individual-level analyses. We define its variables in the separate codebook.
- `cuarteles.csv`: location of military facilities in Spain, coded as explained in the main paper
- `cuarteles1920.csv`: location of military facilities in Spain in 1920, coded from historical archives as explained in the main paper and in the Online Appendix
- `INE_census.csv`: census data for all Spanish municipalities, see [this repository](https://github.com/franvillamil/scrap-INE-census)
- `results1936.csv`: electoral results from 1936 elections, from replication data from Villamil (2021)
- `secc_censal_indic_demograficos.csv`: demographic data at the census section-level
- `secc_censal_renta.csv`: income data at the census section-level
- `ZA2391_v13-0-0.dta.zip`: Politbarometer data, obtained from the GESIS website ([https://search.gesis.org/research_data/ZA2391](https://search.gesis.org/research_data/ZA2391))

In addition, the `func` folder contains a set of R functions predefined that are used throghout the code.

#### Using `make`



---

#### R code

**Data creation:**

- `download_elections`
- `distance_matrix`
- `spatial_lags`
- `spatial_overlay`
- `create_dataset`
- `slm_datajoin`
- `cuarteles1920`

**Main analyses:**

- `lm`
- `lm_diff`
- `slm`
- `slm_tables`
- `survey_analyses`
- `cses`

**Extra:**

- `descriptives`
- `c1920_models`
- `politbarometer`


---

#### OS and software

**Note:** `devtools::install_github("r-lib/svglite")`
