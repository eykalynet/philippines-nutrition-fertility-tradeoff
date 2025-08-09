# Data Analysis – DHS Data

## Overview
This directory contains scripts and outputs for analyzing harmonized Demographic and Health Survey (DHS) data.  
Due to **data access restrictions** imposed by the DHS Program, we cannot upload the raw DHS datasets directly to this repository.

## Accessing the Data
If you wish to replicate the analysis or work with the same datasets, you can request access to the DHS data through the official DHS Program website:

➡ **[Request DHS Data Access](https://dhsprogram.com/data/)**

You will need to:
1. Create an account with the DHS Program.
2. Submit a data request specifying the survey(s) you require.
3. Wait for approval (typically a few business days).

Once approved, you will be able to download the data in your preferred format (e.g., Stata, SPSS, SAS).

## Repository Structure
- `clean-data/` – Processed datasets derived from the raw DHS files (not included here; regenerate after obtaining the raw data).
- `scripts/` – R, Python, or other scripts for cleaning, processing, and analyzing the DHS data.
- `outputs/` – Generated tables, figures, and other analysis results.

## Reproducing the Analysis
After obtaining the raw data:
1. Place the files in the appropriate `raw-data/` folder (create if necessary).
2. Run the scripts in `scripts/` to generate cleaned data and outputs.
3. Outputs will be saved to the `outputs/` directory.

---

**Note:** Please respect the DHS Program’s data usage terms and conditions.
