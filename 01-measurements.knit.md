# Measurement Systems and Integration {#measurements}

This chapter covers how body measurements are captured, stored, and integrated into the parametric pattern drafting system. We use SeamlyMe as the primary measurement tool, but the system accepts any CSV following our format.

## Measurement Philosophy

### Body Measurements vs Pattern Measurements

**Body measurements** are taken directly from the body (or a dress form). They represent the actual dimensions of the person who will wear the garment.

**Pattern measurements** are body measurements plus ease. They represent the dimensions of the finished garment.

Pattern Measurement = Body Measurement + Ease


### Types of Ease

| Ease Type | Purpose | Example |
|-----------|---------|---------|
| **Wearing ease** | Minimum needed for movement and comfort | 4-6 cm at bust |
| **Design ease** | Style choice affecting silhouette | Additional 10 cm for oversized look |
| **Negative ease** | Garment smaller than body (stretch fabrics) | -2 cm at bust for fitted knit |

## SeamlyMe Integration

[SeamlyMe](https://seamly.net/) is open-source software for managing body measurements. It exports measurements in a CSV format we can read directly.

### Export Process from SeamlyMe

1. Open your measurement file (`.vit`) in SeamlyMe
2. Go to **Measurements → Export to CSV**
3. Select all measurements needed for the pattern
4. Save the CSV to `data/measurements/`

### SeamlyMe CSV Format

```csv
code,reference,description,value,formula
height,A01,Height: Total,154,154
bust_circ,G04,Bust circumference,113,113
neck_front_to_waist_f,H01,Neck Front to Waist Front,58,58
```

Columns explained:

    code: Internal identifier used in our R code (e.g., bust_circ)

    reference: SeamlyMe reference code (e.g., G04)

    description: Human-readable description

    value: The actual measurement in centimeters

    formula: Can be a fixed value or a formula referencing other measurements

Required Measurements by Block Type

Female Bodice Block Requirements

# Required measurements for female bodice


``` r
female_bodice_required <- c(
  "height",
  "bust_circ",
  "neck_front_to_waist_f",
  "back_waist_length",
  "shoulder_length",
  "armscye_circ",
  "neck_width",
  "across_chest_f",
  "bustpoint_to_bustpoint",
  "bustpoint_to_neck_side"
)
```



# Create reference table

``` r
reqs_df <- data.frame(
  Code = female_bodice_required,
  Description = c(
    "Total height",
    "Bust circumference",
    "Front waist length (neck to waist)",
    "Back waist length",
    "Shoulder length",
    "Armscye circumference",
    "Neck width",
    "Across chest (front)",
    "Bust point to bust point",
    "Bust point to neck side"
  ),
  Category = c("Reference", "Circumference", "Length", "Length", 
               "Length", "Circumference", "Width", "Width", 
               "Width", "Length")
)

knitr::kable(reqs_df, 
             caption = "Required measurements for female bodice block",
             booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position", "striped"))
```

Male Bodice Block Requirements


``` r
male_bodice_required <- c(
  "height",
  "chest_circ",
  "neck_front_to_waist_f",
  "back_waist_length",
  "shoulder_length",
  "armscye_circ",
  "neck_width",
  "across_back",
  "across_chest_m"
)

reqs_male_df <- data.frame(
  Code = male_bodice_required,
  Description = c(
    "Total height",
    "Chest circumference",
    "Front waist length",
    "Back waist length",
    "Shoulder length",
    "Armscye circumference",
    "Neck width",
    "Across back width",
    "Across chest (male)"
  ),
  Category = c("Reference", "Circumference", "Length", "Length",
               "Length", "Circumference", "Width", "Width", "Width")
)

knitr::kable(reqs_male_df,
             caption = "Required measurements for male bodice block",
             booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position", "striped"))
```


Skirt Block Requirements





















