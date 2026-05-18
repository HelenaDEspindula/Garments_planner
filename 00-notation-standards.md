# Notation Standards and Conventions {#notation-standards}

This chapter defines all notation, terminology, and standards used throughout this pattern making documentation. Following these conventions ensures consistency across all pattern blocks and variations.

## Measurement System

All measurements in this book use the **metric system** exclusively.

| Unit | Abbreviation | Precision | Usage |
|------|-------------|-----------|-------|
| Centimeter | cm | 0.1 | Body measurements, pattern dimensions, ease |
| Millimeter | mm | 0.5 | Notches, seam allowances, small details |
| Meter | m | 0.01 | Fabric length estimation only |

**Critical rule**: Never mix measurement systems. All formulas assume centimeter inputs and return centimeter outputs.

## Coordinate System

All patterns are drafted on a 2D Cartesian coordinate system following textile industry conventions:

- **Origin (0,0)**: Bottom-left corner of the foundation rectangle (point D or C depending on orientation)
- **X-axis**: Horizontal, positive values extend to the right (pattern width)
- **Y-axis**: Vertical, positive values extend upward (pattern length/height)
Y-axis (Height)
^
|
| A(0, height) ─────────── B(width, height)
| │ │
| │ FRONT BACK │
| │ │
| D(0, 0) ─────────────── C(width, 0)
|
+──────────────────────────────────────────> X-axis (Width)
Origin (0,0)



## Point Naming Conventions

Construction points follow traditional pattern drafting conventions with letter designations:

### Primary Structure Points (Foundation Rectangle)
| Point | Description | Typical Position |
|-------|-------------|------------------|
| A | Top-left corner | (0, height) |
| B | Top-right corner | (width, height) |
| C | Bottom-right corner | (width, 0) |
| D | Bottom-left corner | (0, 0) |

### Construction Division Points
| Point | Description | Derivation |
|-------|-------------|------------|
| E, F | Side seam offset | A + offset, D + offset |
| G, H | Center/Armscye division | Midpoint of EB, FC |
| I, J, K, L | Front piece divisions | Divide AG into 4 equal parts |

### Pattern Feature Points
| Category | Points | Represents |
|----------|--------|------------|
| Neckline | N, Decote points | Front and back neckline curves |
| Shoulder | M, P, I, L | Shoulder seam endpoints |
| Armscye | Q, R, S, T, U, V, X, Y, Z | Sleeve cap curve control points |
| Darts | Various | Dart apex and legs |
| Waist/Hip | W, Hip points | Waistline and hip curve points |

## Line Types and Meanings

Based on the [FreeSewing Notation System](https://freesewing.eu/docs/about/notation/), adapted for R/ggplot2:

### Structural Lines
| Line Type | R Linetype | Color | Purpose |
|-----------|-----------|-------|---------|
| **Seam line** | `solid` | Black (#212121) | Actual sewing line |
| **Seam allowance** | `dashed` | Grey (#757575) | Fabric cutting line |
| **Construction line** | `dotted` | Light grey | Temporary guide lines |
| **Grainline** | `dotted` + arrow | Green (#388E3C) | Fabric grain direction |
| **Cut-on-fold** | `twodash` + arrows | Red (#D32F2F) | Place on fabric fold |
| **Center line** | `dashed` | Grey | Center front/back |

### Annotation Lines
| Line Type | R Linetype | Color | Purpose |
|-----------|-----------|-------|---------|
| **Dimension** | `dotted` thin | Blue (#1565C0) | Measurement callouts |
| **Note** | `dashed` thin | Dark grey | Construction notes |
| **Mark** | `solid` thin | Various | Placement indicators |
| **Contrast** | `solid` colored | Cyan (#00BCD4) | Highlight specific areas |
| **Help** | `dotted` thin | Light grey | Assembly guides |

## Notches and Markings

### Notch Types (FreeSewing Standard)
- **Front notch (⊙)**: Circle with dot center — indicates front of garment
- **Back notch (⊗)**: Circle with cross — indicates back of garment

### Construction Marks
| Symbol | R Implementation | Meaning |
|--------|-----------------|---------|
| ⊙ | `add_notch(type="front")` | Front orientation |
| ⊗ | `add_notch(type="back")` | Back orientation |
| ⊕ | `add_button()` | Button placement |
| ⊘ | `add_buttonhole()` | Buttonhole placement |
| ─●─ | `add_bartack()` | Reinforcement stitch |
| ⇅ | `add_grainline()` | Grain direction |

## Pattern Piece Information Block

Every pattern piece must include:

1. **Piece identifier**: Number and name (e.g., "1 — Front Bodice")
2. **Size**: Measurement set identifier
3. **Cutting instructions**: Quantity and fabric type
4. **Date**: Drafting date (ISO format: YYYY-MM-DD)
5. **Scale verification**: 5cm × 5cm test square

## Color Standards

### Fabric Type Colors
| Color Name | Hex Code | R Variable | Usage |
|-----------|----------|------------|-------|
| Primary fabric | #212121 | `fabric_primary` | Main garment fabric |
| Lining | #1976D2 | `fabric_lining` | Lining pieces |
| Interfacing | #F57C00 | `fabric_interfacing` | Interfacing/fusing |

### Annotation Colors
| Purpose | Hex Code | R Variable |
|---------|----------|------------|
| Seam allowance | #757575 | `seam_allowance` |
| Grainline | #388E3C | `grainline` |
| Cut on fold | #D32F2F | `cut_on_fold` |
| Dimensions | #1565C0 | `dimensions` |
| Front notch | #212121 | `notch_default` |
| Back notch | #D32F2F | `notch_back` |

## Grid System

All pattern plots include a reference grid for visual measurement:

- **Major grid lines**: Every 5 cm, solid light grey (#E0E0E0), linewidth 0.3
- **Minor grid lines**: Every 1 cm, very light grey (#F5F5F5), linewidth 0.1
- **Axis labels**: Centered on each major grid line

The grid serves as:
- Quick visual reference for dimensions
- Scale verification tool
- Alignment guide for pattern pieces

## File Naming Conventions
{garment}{block}{version}_{size}.{ext}

Examples:
bodice_front_female_v1.0_size48.pdf
sleeve_basic_v2.1_universal.csv
skirt_pencil_variation_v1.0_size42_A0.pdf



### Version Numbering
- **Major** (X.0.0): Complete redraft or design change
- **Minor** (0.X.0): Significant adjustments to fit
- **Patch** (0.0.X): Small corrections, annotation fixes

## Printing Standards

### Scale Verification
Always print and measure the 5cm test square before cutting. If the square doesn't measure exactly 5cm × 5cm:
1. Check printer settings for "Actual size" (not "Fit to page")
2. Verify PDF scaling is set to 100%
3. Re-print if scale is off by more than 1mm

### Paper Sizes
| Format | Dimensions (cm) | Use Case |
|--------|----------------|----------|
| A4 | 21.0 × 29.7 | Home printing (tiled) |
| A3 | 29.7 × 42.0 | Small patterns |
| A0 | 84.1 × 118.9 | Plotter, full patterns |

### Tile Assembly
For tiled home printing:
1. Trim along registration marks
2. Align matching marks between tiles
3. Tape from center outward
4. Verify scale after assembly

## Measurement File Format

### SeamlyMe CSV Structure
```csv
code,reference,description,value,formula
height,A01,Height: Total,154,154
bust_circ,G04,Bust circumference,113,113
```

### Ease Parameters CSV Structure
```csv
parameter,value,unit,description
bust_ease,6,cm,Overall bust ease
shoulder_slope_front,4,cm,Front shoulder drop
```

## R Implementation Notes

### Function Naming

    calc_*: Pure mathematical calculations (distance, angle)

    create_*: Generate new objects (curves, plots)

    add_*: Modify existing plots with annotations

    read_* / write_*: File I/O operations

    validate_*: Check data integrity

### Coordinate Storage

    Points stored as named vectors: c(x, y)

    Point collections as named lists: list(A = c(0, 44), B = c(48, 44))

    Plotted points as data frames: data.frame(name, x, y)

### Cache Strategy

    All plots cached in images/cache/

    Cache invalidation based on parameter hash

    Manual cache clear: delete images/cache/*.pdf

## References

    FreeSewing Notation Guide: https://freesewing.eu/docs/about/notation/

    ISO 8559: Garment construction and anthropometric surveys

    ABNT NBR 13377: Brazilian standard for garment sizes (where applicable)


