---
title: 'b3gbi: Standardized Biodiversity Indicators from Spatiotemporal Occurrence Cubes'
tags:
  - R
  - biodiversity
  - indicators
  - data-cube
  - ecology
  - uncertainty
authors:
  - name: Shawn Dove
    orcid: 0000-0001-9465-5638
    affiliation: 1
affiliations:
  - name: Department of Animal Ecology & Systematics, Justus Liebig University Giessen, Giessen, Germany
    index: 1
date: 27 June 2026
bibliography: paper.bib
---

# Summary

Standardized and reproducible biodiversity indicators are essential for tracking ecological trends, assessing conservation policies, and reporting on international biodiversity frameworks. However, raw species occurrence records—such as those hosted by the Global Biodiversity Information Facility (GBIF)—are typically highly voluminous, unevenly sampled, and noisy. To address this, the **b3gbi** (B-Cubed General Biodiversity Indicators) R package provides a standardized, automated, and reproducible pipeline to calculate spatial and temporal biodiversity indicators directly from pre-processed spatiotemporal species occurrence cubes [@desmet2023]. The package supports a suite of indicators—including species richness, evenness, rarity, completeness, and turnover—complete with decoupled, robust uncertainty estimation via bootstrapping.

# Statement of Need

A major bottleneck in using global biodiversity datasets for policy reporting is the lack of standardized workflows for converting raw occurrence records into reliable indicators. Occurrence data cubes aggregate occurrences into specific spatial grid systems (e.g., EEA, MGRS, EQDGC, or ISEA3H grids) and temporal units (e.g., years), which significantly reduces data volume and mitigates some spatial and temporal biases [@desmet2023]. 

While packages like `rgbif` [@wallace2020] interface with GBIF data, and packages like `iNEXT` [@inext] calculate specific diversity metrics (such as Hill numbers), there is a distinct gap for a comprehensive, user-friendly framework that takes raw data cubes and automates the entire process of spatial boundary clipping, multi-scale grid aggregation, indicator calculation, and visualization. Furthermore, assessing the reliability of these indicators is critical. `b3gbi` addresses this need by integrating seamlessly with the `dubicube` package [@dubicube] to perform cube-level bootstrapping, allowing users to calculate robust confidence intervals that capture the underlying sampling uncertainty of the original occurrence data.

# Software Design and Workflow

The `b3gbi` package is designed around a clean, three-step pipeline:

```
  +--------------+       +-------------------+       +-----------------------+
  | Ingestion &  |       |     Indicator     |       |      Uncertainty      |
  | Validation   | ----> |    Calculation    | ----> |      Estimation       |
  | process_cube |       |  (wrapper funcs)  |       |        add_ci         |
  +--------------+       +-------------------+       +-----------------------+
```

1. **Ingestion & Validation**: The `process_cube()` function imports spatiotemporal species occurrence cubes (as CSV files), validates the required fields, auto-detects grid types (including gridded coordinate columns), and filters the data by year. The resulting output is structured into a `processed_cube` class.
2. **Indicator Calculation**: A series of wrapper functions calculate indicators over space (`*_map()`) or time (`*_ts()`). Supported indicators include:
   - **Richness**: Observed richness (`obs_richness`), cumulative richness (`cum_richness`), species richness density (`spec_richness_density`), and estimated Hill numbers ($q = 0, 1, 2$) [@chao2014; @inext].
   - **Evenness**: Pielou evenness (`pielou_evenness`) and Williams evenness (`williams_evenness`).
   - **Completeness & Rarity**: Sample completeness/coverage (`completeness`), abundance-based rarity (`ab_rarity`), and area-based rarity (`area_rarity`).
   - **Occupancy & Dynamics**: Species occurrences (`spec_occ`), species range (`spec_range`), relative occupancy (`relative_occupancy`), and species turnover (`occ_turnover`).
3. **Uncertainty & Visualization**: Calling the generic `plot()` function on an indicator object automatically generates clean, publishable map or time-series plots. To calculate uncertainty, the user passes a temporal indicator object to `add_ci()`. Decoupling the intensive bootstrapping process from initial indicator calculations allows for quick data exploration before executing heavy simulations.

# Uncertainty Estimation & Statistical Limitations

When calculating uncertainty via `add_ci()`, users can choose between cube-level bootstrapping (resampling original occurrences) and indicator-level bootstrapping (resampling calculated values) [@dubicube]. Cube-level bootstrapping is the default and most robust level, and `add_ci()` automatically customizes the bootstrap settings based on the indicator type:
- **Hill Numbers**: Because Hill numbers are calculated via `iNEXT`'s internal rarefaction/extrapolation engine, `add_ci()` delegates directly to `iNEXT` to avoid double-bootstrapping.
- **Bounded Indicators**: Evenness indicators automatically use logit transformation to keep confidence intervals within the valid $[0, 1]$ bounds.

Importantly, several indicators are excluded from post-hoc bootstrapping (`noci_list`):
- **Observed Richness and Richness Density**: Resampling from observed occurrences can never discover new/unobserved species, meaning bootstrapped richness estimates are strictly less than or equal to the observed richness ($S_{boot} \le S_{obs}$). This forces the confidence intervals to lie entirely at or below the observed richness, which is statistically incorrect.
- **Sample Completeness**: Sample coverage (completeness) is calculated as a deterministic sample statistic using `iNEXT` methods, and confidence interval estimation is not supported.
- **Relative Occupancy**: Standard bootstrapping is not supported because resampling occurrences strips essential spatial metadata (like `total_num_cells` or `total_area_sqkm`). These spatial attributes cannot be regenerated from occurrences alone because occurrences contain no information about empty grid cells (zero occurrences) in the study area.
- **Cumulative Richness, Species Turnover, and Taxonomic Distinctness**: Standard bootstrapping is inappropriate due to the inherently sequential nature of cumulative counts, the high instability introduced in consecutive species lists, and extreme sensitivity to exact taxonomic compositions.

# Acknowledgements

This package was developed as part of the "Biodiversity Building Blocks for Policy" (B-Cubed) project, which has received funding from the European Union's Horizon Europe Research and Innovation Programme under Grant Agreement No. 101059592.

# References
