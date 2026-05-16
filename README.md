# Data and Code Repository: Fluctuating Asymmetry in Carabid Beetles

This repository contains the datasets, spatial data, raw imagery, and R scripts associated with the research article published in *Global Ecology and Conservation*.

**Associated Publication:**
> **DOI:** [10.1016/j.gecco.2026.e04256](https://doi.org/10.1016/j.gecco.2026.e04256)

---

## Repository Locations

To ensure the long-term preservation of large files and the version control of scripts, the project materials are hosted across two platforms:
*   **Data (Zenodo):** https://zenodo.org/records/20244986 contains `.xlsx` datasets, `.kmz` spatial data, and high-resolution species imagery.
*   **Code (GitHub):** [DomecStoces/Deeplearning](https://github.com/DomecStoces/Deeplearning) contains the R scripts used for statistical modeling and plot generation.

---

## Data Availability (Zenodo)

The Zenodo archive contains three primary components:

**Included Species:**
*   *Harpalus flavicornis* (`flav`)
*   *Ophonus cribricollis* (`ophonus`)
*   *Harpalus picipennis* (`picipennis`)
*   *Harpalus subcylindricus* (`sub`)

### 1. `Measuring_in_xlsx.zip`
This archive contains individual antennal segment measurements (a2, a3, and a4) 
for four carabid beetle species. The datasets are split into two prefixes 
('data_' and 'grubb_') which correspond to specific stages of the statistical pipeline:

* 'grubb_' files (e.g., grubb_flav_a2.xlsx)
    Intermediate datasets structured with side-specific replicate measurements. 
    These are utilized for primary data validation and quality control tasks:
  - Outlier Identification: Evaluated via Grubbs' tests and Interquartile Range (IQR) criteria (following Rivera & Neely, 2020) to separate true developmental instability from extreme anomalies.
  - Distribution Checks: Testing skewness (thresholds within +/-1) and kurtosis boundaries (leptokurtic >3, platykurtic <3).
  - Measurement Error (ME) & Directional Asymmetry (DA): Used inside linear mixed-effects models (REML) following Van Dongen et al. (1999) and Palmer & Strobeck (2003) to confirm that true fluctuating asymmetry variance significantly outweighs background measurement error.

* 'data_' files (e.g., data_flav_a2.xlsx)
    The final cleaned datasets containing absolute asymmetry values (|R - L|), 
    body sizes, and structural parameters passed into the main ecological 
    generalized linear mixed models (GLMMs).

**File List & Sizes inside Measuring_in_xlsx.zip:**
*   `data_flav_a2.xlsx` (51.2 kB), `data_flav_a3.xlsx` (50.9 kB), `data_flav_a4.xlsx` (51.5 kB)
*   `data_ophonus_a2_c.xlsx` (40.1 kB), `data_ophonus_a3.xlsx` (40.0 kB), `data_ophonus_a4.xlsx` (39.9 kB)
*   `data_picipennis_a2.xlsx` (40.7 kB), `data_picipennis_a3.xlsx` (39.9 kB), `data_picipennis_a4.xlsx` (39.7 kB)
*   `data_sub_a2.xlsx` (43.0 kB), `data_sub_a3.xlsx` (47.0 kB), `data_sub_a4.xlsx` (47.0 kB)
*   `grubb_flav_a2.xlsx` (50.2 kB), `grubb_flav_a3.xlsx` (30.1 kB), `grubb_flav_a4.xlsx` (45.3 kB)
*   `grubb_ophonus_a2_c.xlsx` (36.1 kB), `grubb_ophonus_a3.xlsx` (36.3 kB), `grubb_ophonus_a4.xlsx` (36.3 kB)
*   `grubb_picipennis_a2.xlsx` (37.0 kB), `grubb_picipennis_a3.xlsx` (36.3 kB), `grubb_picipennis_a4.xlsx` (36.0 kB)
*   `grubb_sub_a2.xlsx` (40.2 kB), `grubb_sub_a3.xlsx` (27.2 kB), `grubb_sub_a4.xlsx`

### 2. `Solar_parks.kmz`
Contains the GPS coordinates of the sampled solar parks, mapped using Google Earth historical satellite imagery.

### 3. Upper Photos of Target Species
High-resolution images of the four target species (*Ophonus cribricollis*, *Harpalus subcylindricus*, *Harpalus flavicornis*, and *Harpalus picipennis*). 

**Imaging Methodology & Equipment:**
*   **Camera:** CANON EOS 2000D digital camera.
*   **Microscope Setup:** Olympus SZX7 stereomicroscope equipped with an SZX2-TR30 mount and a DFPL 1.5×–4 adapter.
*   **Magnification:** Effective magnification range of approx. 1.0× to 8.4× (0.67× to 5.6× magnification multiplied by 1.5× objective).
*   **Format:** 6000 × 4000 pixels, JPEG format.
*   **Calibration:** Each image includes a calibrated scale caliper with a measurement accuracy of ±0.01 mm.

---

## Code Availability (GitHub)

The GitHub repository ([DomecStoces/Deeplearning](https://github.com/DomecStoces/Deeplearning)) contains the R scripts used for the data analysis and figure generation presented in the manuscript. 

*Note: The intermediate cleaning protocols from DeepLabCut and the scripts used for refining and trimming the Excel datasets have been omitted to maintain the clarity and usability of the final statistical workflows.*

### Scripts Included:

**1. `FA_protocol.R`**
*   **Purpose:** Conducts the Fluctuating Asymmetry (FA) analysis.
*   **Key Operations:**
    *   Calculates skewness, kurtosis, and removes extreme outliers (IQR method) to isolate natural variation in developmental instability.
    *   Tests for Directional Asymmetry (DA), Fluctuating Asymmetry (FA), and Measurement Error (ME) using linear mixed-effects models (REML).
    *   Evaluates dependency of absolute asymmetry (|R-L|) on body size.
    *   Analyzes the influence of Wing morphology (Apterous, Brachypterous, Macropterous), Sex, and Treatment (Control/Extensive grassland vs. Solar park) using generalized linear mixed models (`glmmTMB`, log-link).
    *   Generates predictive model plots and boxplots using `ggplot2`, `emmeans`, and `DHARMa`.

**2. `QQ_plot_year.R`**
*   **Purpose:** Temporal distribution comparison.
*   **Key Operations:**
    *   Extracts log-transformed FA indices across sampling years (2023 vs. 2024).
    *   Generates a Quantile-Quantile (QQ) plot to visually assess the temporal consistency of the asymmetry data.
    *   Performs a Kolmogorov-Smirnov (`ks.test`) to test for distribution differences between years.

---

## Usage Instructions

1.  Download the dataset archive (`Measuring_in_xlsx.zip`) from Zenodo and extract the `.xlsx` files into your working directory.
2.  Clone the GitHub repository: `git clone https://github.com/DomecStoces/Deeplearning.git`
3.  Ensure you have the required R packages installed: `data.table`, `readxl`, `writexl`, `dplyr`, `moments`, `outliers`, `nlme`, `car`, `lme4`, `lmerTest`, `glmmTMB`, `DHARMa`, `emmeans`, `ggplot2`, `ggpubr`.
4.  Run `FA_protocol.R` or `QQ_plot_year.R`. Verify that the file paths in the `read_xlsx()` functions match the location of your extracted datasets.

---

## Citation

If you use the data, images, or code in this repository, please cite the original article:

> Stočes, D., Šipoš, J., Szabó, M. Z., Elek, Z., & Gallé, R. (2026). Dispersal ability modulates fluctuating asymmetry in carabid beetle populations across solar parks and extensive grasslands. *Global Ecology and Conservation*, e04256. https://doi.org/10.1016/j.gecco.2026.e04256
