# 📊 Professional Visualizations: Imaging Projects

This repository contains professional data visualizations and analytics related to clinical imaging and diagnostic data from the ADC Imaging Core and related projects. The scripts and notebooks focus on MRI, PET, longitudinal tracking, and amyloid harmonization.

---

## 📘 Project Summaries

### 🧠 MRIQC Data
Visualizes MRIQC (MRI Quality Control) group-level metrics for Year 3 of the current grant period (2/1/23-1/31/24) for imaging data.

**Script Description:**

|       File     |   Description  |
|:--------------:|:--------------:|
|`ADC_IC_MRIQC_Y3.ipynb` | Summarizes MRIQC metric data |

**Data Files Required:**
- `group_T1w.tsv`
- `group_bold.tsv`

**Tools Used:** `pandas`, `seaborn`, `matplotlib`

**Run:**
```bash
pip install pandas seaborn matplotlib
jupyter notebook ADC_IC_MRIQC_Y3.ipynb
```

---

### 📈 Longitudinal Imaging Data
Tracks scan frequency and types across time for longitudinal participants.

**Script Description:**

|       File     |   Description  |
|:--------------:|:--------------:|
|`Longitudinal_scans.R` | Summarizes imaging scans across time for longitudinal participants |

**Data File Required:**
- `imaging_diagnosis_mastersheet_2024-06-10.xlsx`

**R packages**: `readxl`, `dplyr`, `ggplot2`, `lubridate`

**Run:**
1. Load script
2. Ensure Excel file is alongside script
3. Execute script

---

### 🧠 Clinical Amyloid PET Data

Merges and harmonizes internal and external clinical amyloid PET read data.

**Script Description:**

|       File     |   Description  |
|:--------------:|:--------------:|
|`amyloid_reads.R` | Harmonizes internal and external clinical amyloid PET data |

**Data Files Required:**

- `ImagingCoreDatabase-Stub_DATA_LABELS_2025-11-25_0830.csv`
- `ImagingCoreDatabase-ICPETMetrics_DATA_LABELS_2025-11-25_0920.csv`
- `v_ucberkeley_amyloid_mrifree_gaain.csv`

**R Packages:** `readr`, `dplyr`, `ggplot2`, `stringr`

**Run in RStudio:**
1. Load script
2. Ensure Excel file is alongside script
3. Run top to bottom

---

## 📫 Contact

For questions or collaboration inquiries, please contact [Joshua Pasaye](https://github.com/jep9731).
