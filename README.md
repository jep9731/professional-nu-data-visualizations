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

### 🧠 Clinical PET Data
Summarizes PET scan reads using a master clinical diagnosis sheet.

**Script Description:**

|       File     |   Description  |
|:--------------:|:--------------:|
|`ADC_IC_PET_Reads.ipynb` | Summarizes PET clinical read data |

**Date File Required:**
- `imaging_diagnosis_mastersheet_2024-03-05.xlsx`

**Tools Used:** `pandas`, `seaborn`, `openpyxl`

**Run:**
```bash
pip install pandas seaborn matplotlib openpyxl
jupyter notebook ADC_IC_PET_Reads.ipynb
```

----

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
2. Ensure Excel file is in data/
3. Execute script

---

### 🧠 Clinical Amyloid PET Data

Merges and harmonizes internal and external clinical amyloid PET read data.

**Script Description:**

|       File     |   Description  |
|:--------------:|:--------------:|
|`amyloid_reads.R` | Harmonizes internal and external clinical amyloid PET data |

**Data Files Required:**

- `AlzheimersDiseaseRes-AmyloidPETmetricsADC_DATA_LABELS_2024-10-28_1324.csv`
- `2024-12-09T17_58_12.809Z_clinical_core_export.csv`
- `SCAN Amyloid PET (MRI-free) GAAIN analysis data.csv`

**R Packages:** `readr`, `dplyr`, `ggplot2`, `stringr`

**Run in RStudio:**
1. Load script
2. Place all data files in data/
3. Run top to bottom

## 📫 Contact

For questions or collaboration inquiries, please contact [Joshua Pasaye](https://github.com/jep9731).

---
