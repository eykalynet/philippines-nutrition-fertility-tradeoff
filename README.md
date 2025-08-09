# QQ Trade-Off Natal Investments

## Project Summary

This project examines the **Quantity-Quality Trade-Off** in the Philippines, focusing on how a 2000 policy in Manila — **Executive Order 003**, which banned modern contraceptives in public health facilities — affected family size and postnatal investments in children.

### Research Questions
- How did the contraceptive ban alter family size in Manila households?  
- How did larger family size affect:  
  1. Birth weight  
  2. Vaccination uptake  
- Did effects vary by child, maternal, or household characteristics?

### Hypothesis
Fertility surges from the contraceptive ban reduced child health investments, with sharper negative effects among disadvantaged households.

### Data & Methods
- **Data:** Philippine Demographic and Health Surveys (1998, 2003, 2008)  
- **Sample:** Matched mother–child pairs  
  - **Mothers:** Birth rates & fertility outcomes  
  - **Children (≤ age 5):** Early-child health outcomes  
- **Approach:** Two-stage regression framework:  
  1. **Stage 1:** Estimate effect of contraceptive ban on fertility.  
  2. **Stage 2:** Estimate causal effect of increased family size on child health outcomes.

### Key Findings
- **Family Size:** Mothers in Manila giving birth after 2000 had **0.20 more children ever born** on average.  
- **Child Health:** Each additional sibling at birth corresponded to **nearly 23 fewer vaccines** per child by age 5.  
- **Distribution of Impact:** Vaccine declines were greatest for children from poorer households and mothers with limited education.

### Policy Implications
- Later-born children in large, low-resource families often miss the **8-doses-by-1-year** target set by the Department of Health.  
- Full immunization remains out of reach for many, highlighting the need for targeted interventions.

### Future Work
- Investigate long-term child outcomes beyond vaccination (e.g., stunting, schooling).  
- Test framework in other contexts with different fertility shocks.

---

## Data Access

Due to **DHS Program data use restrictions**, raw DHS datasets are **not included** in this repository.  
To access the data, request it directly from the DHS Program:

➡ **[Request DHS Data Access](https://dhsprogram.com/data/)**

**Steps:**
1. Create a DHS Program account.  
2. Submit a request specifying the surveys and countries needed.  
3. Wait for approval (typically a few business days).  
4. Download the datasets in your preferred format (Stata, SPSS, SAS).

Once approved, place the raw files in a `raw-data/` folder (not tracked by Git) and run the scripts in `data-analysis/scripts/` to reproduce results.

---

## Repository Structure
- `data-analysis/` – Scripts and documentation for cleaning, harmonizing, and analyzing DHS data.  
- `docs/` – Supporting documentation, regression tables, and paper-related materials.  
- `outputs/` – Generated tables, figures, and results from the analysis (no raw DHS data).  
- `Salvador_SchupfPoster_25.pdf` – Research poster summarizing findings.

---

**Poster:**  
[Research Poster (PDF)](Salvador_SchupfPoster_25.pdf)  
*(Click to view or download)*

---

## License
This repository is released under the [MIT License](LICENSE).  
Note: The DHS data itself is **not** covered by this license and remains subject to DHS Program terms of use.

**Disclaimer:**  
The raw DHS data cannot be redistributed. All results in this repository are derived from authorized use of DHS datasets.
