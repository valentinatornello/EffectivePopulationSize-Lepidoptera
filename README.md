# Effective Population Size (Ne) in Laboratory-Reared Lepidoptera

An open-source R toolkit for estimating and monitoring the effective population size (Ne) of insectary colonies of Lepidoptera species used in biological control programs.

---

## Background

Maintaining genetic diversity in laboratory-reared insect colonies is critical for the long-term success of biological control programs. In closed insectary populations, repeated bottlenecks across rearing cycles reduce the **effective population size (Ne)** — the number of individuals that genetically contribute to the next generation — leading to inbreeding accumulation, fitness depression, and loss of adaptive potential.

This repository provides a standardized, reproducible workflow to estimate Ne from routine production records, project inbreeding trajectories, and determine the minimum number of rearing batches required for statistically reliable monitoring.

---

## Objectives

- Estimate demographic Ne from rearing production records (egg viability, larval transfer, pupal collection, adult emergence)
- Estimate sex-ratio Ne using the Wright (1931) formula: $N_e = 4N_fN_m / (N_f + N_m)$
- Calculate harmonic mean Ne across production batches to capture the cumulative effect of population bottlenecks
- Project inbreeding accumulation across generations using the standard model: $F_t = 1 - (1 - 1/2N_e)^t$
- Determine minimum sample sizes for periodic colony monitoring
- Generate fully reproducible scientific reports (R Markdown → PDF)

---

## Species covered

| Species | Family | Crop pest |
|---|---|---|
| *Diatraea saccharalis* | Crambidae | Maize, sugarcane |
| *Spodoptera frugiperda* | Noctuidae | Maize, sorghum |
| *Anticarsia gemmatalis* | Erebidae | Soybean |
| *Helicoverpa zea* | Noctuidae | Maize, cotton |
| *Rachiplusia nu* | Noctuidae | Soybean |
| Other Lepidoptera | — | — |

---

## Methods

| Method | Description |
|---|---|
| **Demographic Ne** | Estimated from adult emergence counts per production batch |
| **Sex-ratio Ne** | Wright (1931) formula accounting for unequal sex ratios |
| **Harmonic mean Ne** | Integrates Ne across batches; penalizes low-Ne bottlenecks |
| **Inbreeding projection** | Accumulation of $F$ over generations at constant Ne |
| **Sample size estimation** | Minimum batches for target precision at 85–95% confidence |
| **Population bottleneck analysis** | Stage-by-stage tracking from egg to adult |

---

## Repository structure

```
EffectivePopulationSize-Lepidoptera/
├── Diatraea saccharalis Ne.r          # Core analysis script
├── Diatraea saccharalis Ne.Rmd        # Reproducible report (PDF output)
├── Diatraea saccharalis population.Rmd
├── analysis.Rmd
└── README.md
```

---

## Requirements

**R** (≥ 4.0) with the following packages:

```r
install.packages(c(
  "tidyverse", "readxl", "lubridate", "knitr",
  "ggplot2", "ggpubr", "tinytex"
))
tinytex::install_tinytex()  # required for PDF rendering
```

---

## Usage

```r
# Set pandoc path if running outside RStudio
Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools")

# Render the report
rmarkdown::render("Diatraea saccharalis Ne.Rmd")
```

---

## Authors

**Valentina Tornello, Carla Serre** — Bayer DSO Insectary, Argentina  
*Lepidopteran colony management and biological control programs*

---

## License

This project is open source. Contributions and adaptations for other insectary species are welcome.
