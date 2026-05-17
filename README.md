# Pattern Making Studies

Parametric pattern drafting documentation using R and Bookdown.

## Overview

This project translates traditional flat pattern drafting instructions into parameterized R code, generating annotated patterns with construction points, measurements, and notes.

## Features

- 📐 Parametric basic blocks (bodice, sleeve, skirt, pants)
- 📏 Metric system throughout
- 🏷 FreeSewing-compatible notation
- 📚 Bookdown documentation with PDF output
- 🖨 Full-scale printable patterns (A0/A4 tiled)
- 📊 Measurement integration with SeamlyMe
- 🔄 Version controlled learning progress

## Quick Start

1. Clone repository
2. Open `pattern-making-textbook.Rproj` in RStudio
3. Run `bookdown::render_book("index.Rmd", "bookdown::pdf_book")`
4. Find PDF in `output/` folder

## Structure
├── *.Rmd # Book chapters
├── R/ # Core functions
├── data/ # Measurements and parameters
├── images/ # Diagrams and cached plots
├── latex/ # LaTeX customization
└── output/ # Generated PDF

## Dependencies

- R >= 4.0
- R packages: bookdown, ggplot2, dplyr, tidyr, readr, knitr, kableExtra
- LaTeX (TeX Live recommended)

## License

MIT

## References

- FreeSewing Notation: https://freesewing.eu/docs/about/notation/
- Seamly2D: https://seamly.net/
