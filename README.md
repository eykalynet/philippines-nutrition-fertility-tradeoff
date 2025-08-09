# QQ Trade-Off Natal Investments

## Overview
This repository contains code, documentation, and outputs for the project **"Trade-Offs in Natal Investments"**.  
The analysis uses harmonized Demographic and Health Survey (DHS) data to examine patterns, determinants, and trade-offs in investments related to natal and early-life health outcomes.

Due to **DHS Program data use restrictions**, the raw survey data is **not included** in this repository.  
However, all scripts and processing steps are provided so you can reproduce the results once you obtain access to the required data.

---

## Repository Structure
- **`data-analysis/`** – Scripts and documentation for cleaning, harmonizing, and analyzing DHS data.
- **`docs/`** – Supporting documentation, regression tables, and paper-related materials.
- **`outputs/`** – Generated tables, figures, and results from the analysis (no raw DHS data).
- **`Salvador_SchupfPoster_25.pdf`** – Research poster summarizing findings.

---

## Accessing the Data
The DHS Program requires users to request access before downloading datasets.  
You can request access here:  
➡ **[DHS Program Data Access Request](https://dhsprogram.com/data/)**

Steps to access:
1. Create a DHS Program account.
2. Submit a request specifying the surveys and countries needed.
3. Wait for approval (typically a few business days).
4. Download the datasets in your preferred format (Stata, SPSS, SAS).

---

## Reproducing the Analysis
1. **Request and download** the required DHS datasets.
2. Place the raw data files in a `raw-data/` folder (not tracked by Git).
3. Run the scripts in `data-analysis/scripts/` to:
   - Harmonize variables across surveys.
   - Clean and prepare the datasets.
   - Produce outputs in `outputs/`.
4. Review the outputs and documentation in `docs/`.

---

## License
This repository is released under the [MIT License](LICENSE).  
Note: The DHS data itself is **not** covered by this license and remains subject to DHS Program terms of use.

---

**Disclaimer:**  
The raw DHS data cannot be redistributed. All results in this repository are derived from authorized use of DHS datasets.
