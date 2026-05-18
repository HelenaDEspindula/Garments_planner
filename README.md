# Pattern Making Studies

Parametric pattern drafting documentation using R and Bookdown, based on traditional methods and modern computational tools.

## 📚 References

This project applies the drafting instructions from:

- **Sophia Jobim** — *O Sistema de Corte e Costura* (The Cutting and Sewing System)
- **Assembil Books** — *How Patterns Work: The Fundamental Principles of Pattern Making and Sewing in Fashion Design*

## 🤖 AI Disclaimer

This project uses AI (DeepSeek) as a coding assistant to help translate traditional pattern drafting instructions into parameterized R code. All generated code is reviewed, tested, and adapted by a human. The design logic and drafting knowledge come from the textbooks listed above.

## 🎯 Project Goals

- **Reproducible patterns**: Change measurements, regenerate the pattern instantly
- **Living documentation**: Annotated diagrams with all construction points, measurements, and notes
- **Learning log**: Version-controlled record of the entire pattern making study journey
- **Future expansion**: Automatic Google Drive saving, drafting animations (Manim), 3D garment visualization

## 📁 Structure

├── *.Rmd # Book chapters
├── Rscripts/ # Core R functions
├── data/ # Measurements and ease parameters
├── images/ # Diagrams and cached plots
├── latex/ # LaTeX customization
└── output/ # Generated PDF and full-scale patterns


## 🚀 Quick Start

1.  Clone the repository
2.  Open `Garments_planner.Rproj` in RStudio
3.  Run `source("Rscripts/install_packages.R")` to install dependencies
4.  Render the book: `bookdown::render_book("index.Rmd", "bookdown::pdf_book")`
5.  Find the PDF in `output/`

## 🔭 Ideas for the Future

- **Google Drive Integration**: Automatically save full-scale PDF patterns to a designated folder.
- **Drafting Animations**: Use Manim (https://www.manim.community/) to create step-by-step animations of the pattern drafting process.
- **3D Garment Simulation**: Generate 3D meshes from the 2D patterns.
- **2D to 3D Transition**: Animate the transformation of a flat pattern into a 3D garment, showing how fit adjustments affect the final shape.

## 📜 License

MIT