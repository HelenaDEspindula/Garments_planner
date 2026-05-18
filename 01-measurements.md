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

\begin{table}[!h]
\centering
\caption{(\#tab:unnamed-chunk-4)Required measurements for skirt block}
\centering
\begin{tabular}[t]{lll}
\toprule
Code & Description & Category\\
\midrule
\cellcolor{gray!10}{waist\_circ} & \cellcolor{gray!10}{Waist circumference} & \cellcolor{gray!10}{Circumference}\\
hip\_circ & Hip circumference & Circumference\\
\cellcolor{gray!10}{waist\_to\_hip} & \cellcolor{gray!10}{Waist to hip distance} & \cellcolor{gray!10}{Length}\\
waist\_to\_knee & Waist to knee distance & Length\\
\cellcolor{gray!10}{height} & \cellcolor{gray!10}{Total height} & \cellcolor{gray!10}{Reference}\\
\bottomrule
\end{tabular}
\end{table}


Sleeve Block Requirements


\begin{table}[!h]
\centering
\caption{(\#tab:unnamed-chunk-5)Required measurements for sleeve block}
\centering
\begin{tabular}[t]{lll}
\toprule
Code & Description & Category\\
\midrule
\cellcolor{gray!10}{armscye\_circ} & \cellcolor{gray!10}{Armscye circumference} & \cellcolor{gray!10}{Circumference}\\
arm\_upper\_circ & Upper arm circumference & Circumference\\
\cellcolor{gray!10}{arm\_elbow\_circ} & \cellcolor{gray!10}{Elbow circumference} & \cellcolor{gray!10}{Circumference}\\
arm\_wrist\_circ & Wrist circumference & Circumference\\
\cellcolor{gray!10}{arm\_shoulder\_tip\_to\_wrist} & \cellcolor{gray!10}{Shoulder to wrist length} & \cellcolor{gray!10}{Length}\\
\addlinespace
arm\_shoulder\_tip\_to\_elbow & Shoulder to elbow length & Length\\
\cellcolor{gray!10}{arm\_armpit\_to\_wrist} & \cellcolor{gray!10}{Armpit to wrist (inside)} & \cellcolor{gray!10}{Length}\\
\bottomrule
\end{tabular}
\end{table}


Pants Block Requirements


\begin{table}[!h]
\centering
\caption{(\#tab:unnamed-chunk-6)Required measurements for pants block}
\centering
\begin{tabular}[t]{lll}
\toprule
Code & Description & Category\\
\midrule
\cellcolor{gray!10}{waist\_circ} & \cellcolor{gray!10}{Waist circumference} & \cellcolor{gray!10}{Circumference}\\
hip\_circ & Hip circumference & Circumference\\
\cellcolor{gray!10}{waist\_to\_hip} & \cellcolor{gray!10}{Waist to hip distance} & \cellcolor{gray!10}{Length}\\
crotch\_depth & Crotch depth (sitting) & Length\\
\cellcolor{gray!10}{inseam} & \cellcolor{gray!10}{Inseam length} & \cellcolor{gray!10}{Length}\\
\addlinespace
outseam & Outseam length & Length\\
\cellcolor{gray!10}{thigh\_circ} & \cellcolor{gray!10}{Thigh circumference} & \cellcolor{gray!10}{Circumference}\\
knee\_circ & Knee circumference & Circumference\\
\cellcolor{gray!10}{ankle\_circ} & \cellcolor{gray!10}{Ankle circumference} & \cellcolor{gray!10}{Circumference}\\
\bottomrule
\end{tabular}
\end{table}


Loading Measurements in R

The `read_measurements()` Function

```
## Key measurements for female size 48:
```

```
##   height                        :  154.0 cm
##   bust_circ                     :  113.0 cm
##   neck_front_to_waist_f         :   58.0 cm
##   shoulder_length               :   10.5 cm
```


Creating a Measurement Template

If you don't have SeamlyMe, you can create a template CSV and fill in values manually:



Measurement Validation

Checking Required Measurements


```
## ✗ objeto 'female_bodice_required' não encontrado
```


```
## Consistency checks:
```

```
##   ✓ waist_vs_bust
##   ✓ hip_vs_waist
##   ✓ arm_segments
```




Ease Parameters

Ease parameters are stored separately from body measurements.

This separation allows:

    Changing body measurements without affecting design decisions

    Experimenting with different fits on the same body

    Sharing ease settings across measurement sets

Ease Parameters File Format





Ease by Garment Type and Fit
Garment	Close Fit	Semi-Fit	Loose Fit	Oversized
Bodice bust	4-6 cm	6-8 cm	8-12 cm	12-20 cm
Bodice waist	2-4 cm	4-6 cm	6-10 cm	10-16 cm
Skirt hip	2-4 cm	4-6 cm	6-8 cm	8-12 cm
Pants hip	2-4 cm	4-6 cm	6-8 cm	—
Sleeve biceps	2-4 cm	4-6 cm	6-8 cm	8-12 cm
Standard Size Reference Tables
Female Standard Measurements (ABNT NBR 13377)



\begin{table}[!h]
\centering
\caption{(\#tab:unnamed-chunk-12)Standard female measurements (cm) — Reference only}
\centering
\begin{tabular}[t]{lrrrrr}
\toprule
Size & Bust & Waist & Hip & Neck\_to\_Waist\_F & Back\_Waist\\
\midrule
\cellcolor{gray!10}{38} & \cellcolor{gray!10}{84} & \cellcolor{gray!10}{64} & \cellcolor{gray!10}{92} & \cellcolor{gray!10}{44} & \cellcolor{gray!10}{38}\\
40 & 88 & 68 & 96 & 45 & 39\\
\cellcolor{gray!10}{42} & \cellcolor{gray!10}{92} & \cellcolor{gray!10}{72} & \cellcolor{gray!10}{100} & \cellcolor{gray!10}{46} & \cellcolor{gray!10}{40}\\
44 & 96 & 76 & 104 & 47 & 41\\
\cellcolor{gray!10}{46} & \cellcolor{gray!10}{102} & \cellcolor{gray!10}{82} & \cellcolor{gray!10}{110} & \cellcolor{gray!10}{48} & \cellcolor{gray!10}{42}\\
\addlinespace
48 & 108 & 88 & 116 & 50 & 43\\
\cellcolor{gray!10}{50} & \cellcolor{gray!10}{114} & \cellcolor{gray!10}{94} & \cellcolor{gray!10}{122} & \cellcolor{gray!10}{52} & \cellcolor{gray!10}{44}\\
\bottomrule
\end{tabular}
\end{table}

Male Standard Measurements

\begin{table}[!h]
\centering
\caption{(\#tab:unnamed-chunk-13)Standard male measurements (cm) — Reference only}
\centering
\begin{tabular}[t]{lrrrr}
\toprule
Size & Chest & Waist & Hip & Back\_Waist\\
\midrule
\cellcolor{gray!10}{46} & \cellcolor{gray!10}{92} & \cellcolor{gray!10}{78} & \cellcolor{gray!10}{94} & \cellcolor{gray!10}{44}\\
48 & 96 & 82 & 98 & 45\\
\cellcolor{gray!10}{50} & \cellcolor{gray!10}{100} & \cellcolor{gray!10}{86} & \cellcolor{gray!10}{102} & \cellcolor{gray!10}{46}\\
52 & 104 & 90 & 106 & 47\\
\cellcolor{gray!10}{54} & \cellcolor{gray!10}{108} & \cellcolor{gray!10}{96} & \cellcolor{gray!10}{110} & \cellcolor{gray!10}{48}\\
\addlinespace
56 & 112 & 102 & 114 & 49\\
\bottomrule
\end{tabular}
\end{table}


Practical Measurement Tips
Taking Body Measurements

    Use a flexible tape measure — not a metal one

    Measure over undergarments — or the clothing layer the garment will be worn over

    Keep the tape parallel to the floor — especially for circumferences

    Don't pull too tight — the tape should be snug but not compressing

    Mark landmarks — use elastic bands to mark waist, hip, and bust levels

    Take multiple measurements — average three readings for accuracy

    Record posture — note if the subject has forward shoulders, sway back, etc.

Measurement Landmarks
Landmark	How to Find
Waist level	Side bend — where the body creases
Hip level	Widest part of hips, usually 18-22 cm below waist
Bust point	Apex of bust (nipple level)
Neck base	Where neck meets shoulder
Shoulder tip	Acromion bone (bony point at shoulder edge)
Elbow	Olecranon process (bony point of elbow)
Wrist	Styloid process (bony bump at wrist)
Chapter Summary

    All measurements in centimeters, stored in SeamlyMe CSV format

    Measurements are validated for presence and consistency before drafting

    Ease parameters are stored separately for flexibility

    Standard size tables provided for reference only — use actual body measurements when possible

    Each block type has specific measurement requirements documented above




