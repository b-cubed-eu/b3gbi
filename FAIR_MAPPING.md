# FAIR Data Specification and Vocabulary Mapping

This document provides the formal semantic mapping and vocabulary definitions for the data structures output by the `b3gbi` R package, ensuring compliance with the Research Data Alliance (RDA) FAIR Data Maturity Model standards (specifically **RDA-I2-01D** and **RDA-I3-03M**).

## 1. Core Dimensions & Provenance

The spatial and temporal frameworks utilized by `b3gbi` are inherited directly from standardized community schemas.

| Column Name | Schema / Vocabulary Alignment | Definition & Context |
| :--- | :--- | :--- |
| `cellCode` | **GBIF Data Cube Specification** / Darwin Core (`dwc:locationID`) | The standardized spatial grid identifier string generated natively by GBIF during data cube extraction. |
| `cellid` | *Internal Software Runtime Metric* | An internal integer index used strictly for programmatic rows-mapping and execution optimization within the R runtime environment. Exempt from external ontology requirements. |
| `year` | **ISO 8601** / Darwin Core (`dwc:year`) | The temporal dimension tracking the calendar year of the data slice, formatted as a standard four-digit year. |

## 2. Calculated Biodiversity Metrics

Calculated outputs are structurally compliant with the Darwin Core **MeasurementOrFact** extension framework.

| Column Name | Schema / Vocabulary Alignment | Definition & Context |
| :--- | :--- | :--- |
| `diversity_val` | Darwin Core (`dwc:measurementValue`) | The primary quantitative value calculated by the package function. The exact semantic property (e.g., Species Richness, Shannon Evenness, Completeness) is defined by the specific function executed by the user. |

## 3. Statistical Interval Metadata

When generating confidence intervals, `b3gbi` passes through metadata structures inherited from the community-standard `iNEXT` (Interpolation and Extrapolation) software ecosystem.

| Column Name | Schema / Vocabulary Alignment | Definition & Context |
| :--- | :--- | :--- |
| `int_type` | **iNEXT Software Schema** / Methodological Vocabulary | The specific statistical methodology used to derive the interval (e.g., `"Asymptotic"` or `"Non-asymptotic"` estimation). |
| `conf_level` | Standard Statistical Vocabulary | The threshold string or numeric indicating the confidence interval width (e.g., `0.95` for a 95% confidence interval). |
| `ll` | Darwin Core Extension Property | The lower confidence limit calculated for the corresponding metric value stored in `diversity_val`. |
| `ul` | Darwin Core Extension Property | The upper confidence limit calculated for the corresponding metric value stored in `diversity_val`. |

---

## 4. Institutional & Project Framework Context

* **Project Umbrella:** This package was developed as a core indicator framework tool under the **B-Cubed (Biodiversity Data Cubes for Biodiversity Indicators)** European Union horizon initiative.
* **Target Environment:** Metadata profiles generated upon asset deposition into persistent repositories (e.g., Zenodo) are programmatically mapped directly onto the **DataCite Metadata Schema** and **Schema.org** (`SoftwareApplication`) semantic networks.
