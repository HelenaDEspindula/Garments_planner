# Pattern Making Studies

Parametric pattern drafting documentation using R and Bookdown, based on traditional methods and modern computational tools.

## About This Book

This book documents my journey learning pattern making through a parametric, code-based approach. Rather than traditional paper drafting, I translate pattern making instructions into R code, creating:

- Reproducible patterns that adjust to any measurement set
- Fully annotated diagrams with all construction points visible
- A searchable reference of drafting techniques
- A record of errors, corrections, and learning insights

## Why Parametric Pattern Making?

Traditional pattern making relies on manual drafting for each size and design. This approach:

1. **Eliminates manual grading**: Change measurements, regenerate the pattern
2. **Preserves design logic**: Every calculation is documented in code
3. **Enables experimentation**: Test design changes instantly
4. **Creates a knowledge base**: Build upon previous work systematically

## Tools Used

- **R** and **R Markdown/Bookdown**: Documentation and computation
- **SeamlyMe**: Body measurement management
- **FreeSewing Notation**: Standard pattern marking symbols
- **Git/GitHub**: Version control and sharing

## How to Use This Book

The book follows a structured learning path:

1. **Chapter 0**: Notation standards and conventions
2. **Chapter 1**: Measurement systems and file formats
3. **Chapters 2a-2f**: Basic blocks for different garments
4. **Chapter 3**: Design variations from basic blocks
5. **Chapter 4**: Real projects with iterations and corrections
6. **Chapter 5**: Reference tables and formulas

## References

This project applies the drafting instructions from:

- **Sophia Jobim** -- *O Sistema de Corte e Costura* (The Cutting and Sewing System)
- **Assembil Books** -- *How Patterns Work: The Fundamental Principles of Pattern Making and Sewing in Fashion Design*

## AI Disclaimer

This project uses AI (DeepSeek) as a coding assistant to help translate traditional pattern drafting instructions into parameterized R code. All generated code is reviewed, tested, and adapted by a human. The design logic and drafting knowledge come from the textbooks listed above.

## Project Goals

- **Reproducible patterns**: Change measurements, regenerate the pattern instantly
- **Living documentation**: Annotated diagrams with all construction points, measurements, and notes
- **Learning log**: Version-controlled record of the entire pattern making study journey
- **Future expansion**: Automatic Google Drive saving, drafting animations (Manim), 3D garment visualization

## Structure

```
├── *.Rmd              # Book chapters
├── Rscripts/          # Core R functions
├── data/              # Measurements and ease parameters
├── images/            # Diagrams and cached plots
├── latex/             # LaTeX customization
└── output/            # Generated PDF and full-scale patterns
```

## Quick Start

1. Clone the repository
2. Open `Garments_planner.Rproj` in RStudio
3. Run `source("Rscripts/install_packages.R")` once to install dependencies
4. Run `source("Rscripts/make_book.R")` to render the book
5. Find the PDF in `output/`

## Ideas for the Future

- **Google Drive Integration**: Automatically save full-scale PDF patterns to a designated folder
- **Drafting Animations**: Use Manim to create step-by-step animations of the pattern drafting process
- **3D Garment Simulation**: Generate 3D meshes from the 2D patterns
- **2D to 3D Transition**: Animate the transformation of a flat pattern into a 3D garment, showing how fit adjustments affect the final shape

## Prerequisites

- R (>= 4.0) and RStudio
- LaTeX distribution (for PDF output)
- Basic understanding of garment construction
- Optional: SeamlyMe for measurement management

## Measurement File Format

### SeamlyMe CSV Structure
```
code,reference,description,value,formula
height,A01,Height: Total,154,154
bust_circ,G04,Bust circumference,113,113
```

### Ease Parameters CSV Structure
```
parameter,value,unit,description
bust_ease,6,cm,Overall bust ease
shoulder_slope_front,4,cm,Front shoulder drop
```

## R Implementation Notes

### Function Naming
- `calc_*`: Pure mathematical calculations (distance, angle)
- `create_*`: Generate new objects (curves, plots)
- `add_*`: Modify existing plots with annotations
- `read_*` / `write_*`: File I/O operations
- `validate_*`: Check data integrity

### Coordinate Storage
- Points stored as named vectors: `c(x, y)`
- Point collections as named lists: `list(A = c(0, 44), B = c(48, 44))`
- Plotted points as data frames: `data.frame(name, x, y)`

### Cache Strategy
- All plots cached in `images/cache/`
- Cache invalidation based on parameter hash
- Manual cache clear: delete `images/cache/*.pdf`

### References

- FreeSewing Notation Guide: https://freesewing.eu/docs/about/notation/
- ISO 8559: Garment construction and anthropometric surveys
- ABNT NBR 13377: Brazilian standard for garment sizes (where applicable)

## Acknowledgments

- FreeSewing.org for the notation system
- Traditional pattern making textbooks
- Open source community

## License

MIT
