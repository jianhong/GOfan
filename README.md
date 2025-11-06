# `GOfan`: Annotating Genomic Regions Through Chromatin Interaction Links

## Overview

`GOfan` provides an intuitive approach to visualize Gene Ontology (GO) enrichment results. By converting complex GO DAGs into clean, circular representations, it allows researchers to quickly grasp the hierarchical structure and biological significance of enriched terms. The interactive and customizable visualizations facilitate exploration of key GO categories, enhancing interpretation and presentation of enrichment analyses.

## Key Features

1. Circular Visualization of GO DAGs – Converts complex GO hierarchies into clear, fan- or sunburst-shaped plots, making enrichment results easy to interpret.

2. Flexible Customization – Supports multiple data layers (e.g., q-values, gene ratios), color mapping, and saving in various formats via ggplot2.

## Installation

### Install from GitHub

```{r}
if (!requireNamespace("remotes")) install.packages("remotes")

remotes::install_github("jianhong/GOfan")
```

## Contributing

Contributions, suggestions, and bug reports are welcome!
Please open an issue or submit a pull request on [GitHub](https://github.com/jianhong/GOfan/issues).
