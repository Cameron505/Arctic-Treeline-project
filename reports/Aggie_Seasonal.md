Aggie subset data
================
2023-05-09

## sample summary

<details>
<summary>
click to open
</summary>

Soils were collected around trees on treelines in the western brooks
range Alaska varying in soil moisture. Three sites were chosen: Tussock
tundra (Mesic), Wet Sedge (Hydric), and Dryas-lichen tundra (Xeric). 8
similar trees were chosen per treatment based on their DBH and proximity
to the treeline. Snow fences were constructed as a treatment to build
larger snow packs around the given trees in winter and compared against
a control group where nothing was done besides sampling. In order to
reduce impact on these tree-plots 8 Ancillary trees were also chosen
based on the same DBH, proximity to treeline parameters and used for
seasonal sampling.  
A previous project conducted at these sites fertilized soils around
similar trees. These soils were also sampled to identify long term
effects of fertilization. Soils were collected from control and
snowfence trees in march and late may/early June (Just after thaw) along
with resin strips (Except in 2020). Soil pore water was taken regularly
during the growing season (2017-2019). During 2019 collection
frequencies reduced due to staffing and were disrupted due to COVID in
2020 with an inability to visit the field sites.

</details>

------------------------------------------------------------------------

## Seasonal (Pore water and extractions)

<details>
<summary>
click to open
</summary>

#### Extractable concentrations:

Ancillary plots only Here are the concentrations for soil K2SO4
extractable nitrate, phosphate, ammonium, total free primary amines,
phenolics, and total reducing sugars. Samples were taken in 2017, 2018
and 2019 at several time points. Due to COVID sample frequency reduced
significantly in 2020, and 2021. Note that some soils were collected in
2022, however none from the ancillary plots.

<details>
<summary>
click to open
</summary>

<img src="Aggie_Seasonal_files/figure-gfm/unnamed-chunk-1-1.png" width="100%" /><img src="Aggie_Seasonal_files/figure-gfm/unnamed-chunk-1-2.png" width="100%" /><img src="Aggie_Seasonal_files/figure-gfm/unnamed-chunk-1-3.png" width="100%" /><img src="Aggie_Seasonal_files/figure-gfm/unnamed-chunk-1-4.png" width="100%" /><img src="Aggie_Seasonal_files/figure-gfm/unnamed-chunk-1-5.png" width="100%" /><img src="Aggie_Seasonal_files/figure-gfm/unnamed-chunk-1-6.png" width="100%" />

</details>

###### Extractable LME:

<details>
<summary>
click to open
</summary>

| analyte   | variable        | numDF | denDF |     F-value |   p_value | p_value == round(p_value, 3) | asterisk |
|:----------|:----------------|------:|------:|------------:|----------:|:-----------------------------|:---------|
| NH4       | MONTH           |     1 |   299 |   4.4976102 | 0.0347652 | FALSE                        | \*       |
| NH4       | YEAR            |     1 |   299 |  11.2811566 | 0.0008844 | FALSE                        | \*       |
| NH4       | Site            |     2 |   299 |  10.7628418 | 0.0000306 | FALSE                        | \*       |
| NH4       | MONTH:YEAR      |     1 |   299 |  25.8473424 | 0.0000007 | FALSE                        | \*       |
| NH4       | MONTH:Site      |     2 |   299 |   3.1565002 | 0.0439968 | FALSE                        | \*       |
| NH4       | YEAR:Site       |     2 |   299 |   0.1679324 | 0.8454906 | FALSE                        | NA       |
| NH4       | MONTH:YEAR:Site |     2 |   299 |   9.9729752 | 0.0000641 | FALSE                        | \*       |
| NO3       | MONTH           |     1 |   299 |  15.8736994 | 0.0000851 | FALSE                        | \*       |
| NO3       | YEAR            |     1 |   299 | 191.6510932 | 0.0000000 | TRUE                         | \*       |
| NO3       | Site            |     2 |   299 |   5.4194678 | 0.0048754 | FALSE                        | \*       |
| NO3       | MONTH:YEAR      |     1 |   299 |  79.7173912 | 0.0000000 | TRUE                         | \*       |
| NO3       | MONTH:Site      |     2 |   299 |   2.7517372 | 0.0654336 | FALSE                        | NA       |
| NO3       | YEAR:Site       |     2 |   299 |   0.7235968 | 0.4858520 | FALSE                        | NA       |
| NO3       | MONTH:YEAR:Site |     2 |   299 |   0.5220105 | 0.5938662 | FALSE                        | NA       |
| PO4       | MONTH           |     1 |   297 |   4.3851250 | 0.0371013 | FALSE                        | \*       |
| PO4       | YEAR            |     1 |   297 |   8.9415670 | 0.0030209 | FALSE                        | \*       |
| PO4       | Site            |     2 |   297 |  15.4792652 | 0.0000004 | FALSE                        | \*       |
| PO4       | MONTH:YEAR      |     1 |   297 |   3.1812235 | 0.0755099 | FALSE                        | NA       |
| PO4       | MONTH:Site      |     2 |   297 |   1.6149417 | 0.2006439 | FALSE                        | NA       |
| PO4       | YEAR:Site       |     2 |   297 |   0.7670401 | 0.4653033 | FALSE                        | NA       |
| PO4       | MONTH:YEAR:Site |     2 |   297 |   6.8148330 | 0.0012772 | FALSE                        | \*       |
| TFPA      | MONTH           |     1 |   295 |   4.3596276 | 0.0376579 | FALSE                        | \*       |
| TFPA      | YEAR            |     1 |   295 |   0.9365378 | 0.3339626 | FALSE                        | NA       |
| TFPA      | Site            |     2 |   295 |   8.9978631 | 0.0001610 | FALSE                        | \*       |
| TFPA      | MONTH:YEAR      |     1 |   295 |  10.7394407 | 0.0011740 | FALSE                        | \*       |
| TFPA      | MONTH:Site      |     2 |   295 |   2.2879118 | 0.1032761 | FALSE                        | NA       |
| TFPA      | YEAR:Site       |     2 |   295 |   1.0357558 | 0.3562452 | FALSE                        | NA       |
| TFPA      | MONTH:YEAR:Site |     2 |   295 |   8.6859168 | 0.0002161 | FALSE                        | \*       |
| TRS       | MONTH           |     1 |   299 |   5.1331611 | 0.0241880 | FALSE                        | \*       |
| TRS       | YEAR            |     1 |   299 |   0.4554783 | 0.5002663 | FALSE                        | NA       |
| TRS       | Site            |     2 |   299 |   3.7207078 | 0.0253453 | FALSE                        | \*       |
| TRS       | MONTH:YEAR      |     1 |   299 |   6.6029744 | 0.0106654 | FALSE                        | \*       |
| TRS       | MONTH:Site      |     2 |   299 |   0.0846192 | 0.9188841 | FALSE                        | NA       |
| TRS       | YEAR:Site       |     2 |   299 |   0.7870434 | 0.4561293 | FALSE                        | NA       |
| TRS       | MONTH:YEAR:Site |     2 |   299 |   1.3061226 | 0.2724092 | FALSE                        | NA       |
| phenolics | MONTH           |     1 |   300 |   7.9247530 | 0.0051990 | FALSE                        | \*       |
| phenolics | YEAR            |     1 |   300 |  19.5917325 | 0.0000134 | FALSE                        | \*       |
| phenolics | Site            |     2 |   300 |   2.2015022 | 0.1124212 | FALSE                        | NA       |
| phenolics | MONTH:YEAR      |     1 |   300 |   3.4818756 | 0.0630201 | FALSE                        | NA       |
| phenolics | MONTH:Site      |     2 |   300 |   0.9607031 | 0.3837977 | FALSE                        | NA       |
| phenolics | YEAR:Site       |     2 |   300 |   1.8956076 | 0.1520220 | FALSE                        | NA       |
| phenolics | MONTH:YEAR:Site |     2 |   300 |   1.2762146 | 0.2806025 | FALSE                        | NA       |

</details>

#### Seasonal pore water concentrations:

Pore water was collected from around the crown of trees in northwest
Alaska to obtain a seasonal perspective of nutrient flow. 2017, 2018 and
2019 had many samples collected, however due to restrictions in travel
during/after COVID only a single set of pore water measurements were
taken. We will likely remove the 2020 and 2021 pore water samples from
the data set.

<details>
<summary>
click to open
</summary>

<img src="Aggie_Seasonal_files/figure-gfm/unnamed-chunk-3-1.png" width="100%" /><img src="Aggie_Seasonal_files/figure-gfm/unnamed-chunk-3-2.png" width="100%" /><img src="Aggie_Seasonal_files/figure-gfm/unnamed-chunk-3-3.png" width="100%" /><img src="Aggie_Seasonal_files/figure-gfm/unnamed-chunk-3-4.png" width="100%" /><img src="Aggie_Seasonal_files/figure-gfm/unnamed-chunk-3-5.png" width="100%" />

</details>

###### Seasonal pore water LME:

<details>
<summary>
click to open
</summary>

| analyte | variable  | numDF | denDF |     F-value | p_value | asterisk |
|:--------|:----------|------:|------:|------------:|--------:|:---------|
| Mass    | MONTH     |     1 |  2208 |   0.0612205 |   0.805 | NA       |
| Mass    | YEAR      |     1 |  2208 |   1.8015229 |   0.180 | NA       |
| Mass    | Site      |     2 |  2208 |   1.2360672 |   0.291 | NA       |
| Mass    | treatment |     2 |  2208 |   1.1057461 |   0.331 | NA       |
| NH4     | MONTH     |     1 |  2243 |  26.0401282 |   0.000 | \*       |
| NH4     | YEAR      |     1 |  2243 | 286.4451963 |   0.000 | \*       |
| NH4     | Site      |     2 |  2243 |   0.1469776 |   0.863 | NA       |
| NH4     | treatment |     2 |  2243 |   1.3044999 |   0.272 | NA       |
| NO3     | MONTH     |     1 |  2206 |   0.0094192 |   0.923 | NA       |
| NO3     | YEAR      |     1 |  2206 |  99.9662866 |   0.000 | \*       |
| NO3     | Site      |     2 |  2206 |  34.7994825 |   0.000 | \*       |
| NO3     | treatment |     2 |  2206 |   5.2813921 |   0.005 | \*       |
| PO4     | MONTH     |     1 |  2178 |  30.1680789 |   0.000 | \*       |
| PO4     | YEAR      |     1 |  2178 | 379.1874157 |   0.000 | \*       |
| PO4     | Site      |     2 |  2178 |   4.1794646 |   0.015 | \*       |
| PO4     | treatment |     2 |  2178 |   1.3890078 |   0.250 | NA       |
| TFPA    | MONTH     |     1 |  2130 |   4.6966325 |   0.030 | \*       |
| TFPA    | YEAR      |     1 |  2130 |   0.0016565 |   0.968 | NA       |
| TFPA    | Site      |     2 |  2130 |   4.7275359 |   0.009 | \*       |
| TFPA    | treatment |     2 |  2130 |   0.8810253 |   0.415 | NA       |
| TRS     | MONTH     |     1 |  2231 |  41.5239057 |   0.000 | \*       |
| TRS     | YEAR      |     1 |  2231 | 114.7911306 |   0.000 | \*       |
| TRS     | Site      |     2 |  2231 |   4.8212145 |   0.008 | \*       |
| TRS     | treatment |     2 |  2231 |   6.1957137 |   0.002 | \*       |

</details>

#### Microbial biomass

Microbial biomass measurements were collected using CHCL3 fumigation
method along side the K2SO4 extracts. Significant changes in microbial
biomass over time and between sites, including a biomass crash observed
in 2018, biomass was not seen increasing again until the end of 2019.

<details>
<summary>
click to open
</summary>
<img src="Aggie_Seasonal_files/figure-gfm/unnamed-chunk-5-1.png" width="100%" /><img src="Aggie_Seasonal_files/figure-gfm/unnamed-chunk-5-2.png" width="100%" /><img src="Aggie_Seasonal_files/figure-gfm/unnamed-chunk-5-3.png" width="100%" />
</details>

###### Microbial biomass LME:

<details>
<summary>
click to open LME results
</summary>

| analyte | variable        | numDF | denDF |   F-value |   p_value | p_value == round(p_value, 3) | asterisk |
|:--------|:----------------|------:|------:|----------:|----------:|:-----------------------------|:---------|
| MBC     | Site            |     2 |   266 |  8.324702 | 0.0003114 | FALSE                        | \*       |
| MBC     | MONTH:YEAR      |     1 |   266 | 12.319021 | 0.0005264 | FALSE                        | \*       |
| MBC     | MONTH:Site      |     2 |   266 |  4.075738 | 0.0180513 | FALSE                        | \*       |
| MBC     | YEAR:Site       |     2 |   266 |  5.834864 | 0.0033110 | FALSE                        | \*       |
| MBC     | MONTH:YEAR:Site |     2 |   266 |  4.331849 | 0.0140828 | FALSE                        | \*       |
| MBN     | YEAR            |     1 |   266 | 83.921320 | 0.0000000 | TRUE                         | \*       |
| MBN     | MONTH:Site      |     2 |   266 | 12.707745 | 0.0000054 | FALSE                        | \*       |
| MBN     | YEAR:Site       |     2 |   266 |  5.680923 | 0.0038375 | FALSE                        | \*       |
| MBN     | MONTH:YEAR:Site |     2 |   266 |  9.534462 | 0.0001002 | FALSE                        | \*       |
| Mic.PO4 | YEAR            |     1 |   269 | 20.701633 | 0.0000081 | FALSE                        | \*       |

Biomass LME significant comparisons

| analyte | variable        | numDF | denDF |    F-value |   p_value | p_value == round(p_value, 3) | asterisk |
|:--------|:----------------|------:|------:|-----------:|----------:|:-----------------------------|:---------|
| MBC     | MONTH           |     1 |   266 |  0.2964890 | 0.5865481 | FALSE                        | NA       |
| MBC     | YEAR            |     1 |   266 |  0.6913539 | 0.4064500 | FALSE                        | NA       |
| MBC     | Site            |     2 |   266 |  8.3247024 | 0.0003114 | FALSE                        | \*       |
| MBC     | MONTH:YEAR      |     1 |   266 | 12.3190213 | 0.0005264 | FALSE                        | \*       |
| MBC     | MONTH:Site      |     2 |   266 |  4.0757381 | 0.0180513 | FALSE                        | \*       |
| MBC     | YEAR:Site       |     2 |   266 |  5.8348642 | 0.0033110 | FALSE                        | \*       |
| MBC     | MONTH:YEAR:Site |     2 |   266 |  4.3318485 | 0.0140828 | FALSE                        | \*       |
| MBN     | MONTH           |     1 |   266 |  3.4767076 | 0.0633397 | FALSE                        | NA       |
| MBN     | YEAR            |     1 |   266 | 83.9213202 | 0.0000000 | TRUE                         | \*       |
| MBN     | Site            |     2 |   266 |  1.9146197 | 0.1494237 | FALSE                        | NA       |
| MBN     | MONTH:YEAR      |     1 |   266 |  0.8526888 | 0.3566297 | FALSE                        | NA       |
| MBN     | MONTH:Site      |     2 |   266 | 12.7077452 | 0.0000054 | FALSE                        | \*       |
| MBN     | YEAR:Site       |     2 |   266 |  5.6809232 | 0.0038375 | FALSE                        | \*       |
| MBN     | MONTH:YEAR:Site |     2 |   266 |  9.5344616 | 0.0001002 | FALSE                        | \*       |
| Mic.PO4 | MONTH           |     1 |   269 |  1.1502124 | 0.2844659 | FALSE                        | NA       |
| Mic.PO4 | YEAR            |     1 |   269 | 20.7016330 | 0.0000081 | FALSE                        | \*       |
| Mic.PO4 | Site            |     2 |   269 |  1.0203188 | 0.3618707 | FALSE                        | NA       |
| Mic.PO4 | MONTH:YEAR      |     1 |   269 |  3.4249215 | 0.0653156 | FALSE                        | NA       |
| Mic.PO4 | MONTH:Site      |     2 |   269 |  2.1748958 | 0.1156139 | FALSE                        | NA       |
| Mic.PO4 | YEAR:Site       |     2 |   269 |  2.2689271 | 0.1053990 | FALSE                        | NA       |
| Mic.PO4 | MONTH:YEAR:Site |     2 |   269 |  1.4283485 | 0.2415165 | FALSE                        | NA       |

Biomass LME all comparisons

</details>
</details>

## Resin Strips

<details>
<summary>
click to open
</summary>

#### Resin strip data by site:

Resin stips showed significant differences between sites. Primary
feature: Xeric contained high NO3, and Mesic contained high PO4. These
differences were not seen in soil extractions, in fact Mesic showed the
highest NO3 extractable concentrations consistently, and Hydric showed
the highest po4 extractable concentrations consistently.

<details>
<summary>
click to open
</summary>

<img src="Aggie_Seasonal_files/figure-gfm/unnamed-chunk-7-1.png" width="100%" /><img src="Aggie_Seasonal_files/figure-gfm/unnamed-chunk-7-2.png" width="100%" /><img src="Aggie_Seasonal_files/figure-gfm/unnamed-chunk-7-3.png" width="100%" />

</details>

###### Resin strip ANOVA:

<details>
<summary>
click to open ANOVA stats
</summary>

| analyte   | YEAR | Purpose2 |   p.value | asterisk |
|:----------|-----:|:---------|----------:|:---------|
| Ammonium  | 2018 | GS       | 0.0000000 | \*       |
| Ammonium  | 2019 | OW       | 0.0000003 | \*       |
| Ammonium  | 2019 | GS       | 0.0000644 | \*       |
| Nitrate   | 2017 | OW       | 0.0223603 | \*       |
| Nitrate   | 2018 | OW-GS    | 0.0425072 | \*       |
| Nitrate   | 2018 | GS       | 0.0172312 | \*       |
| Nitrate   | 2020 | OW-GS    | 0.0011635 | \*       |
| Nitrate   | 2021 | OW       | 0.0000000 | \*       |
| Phosphate | 2017 | OW       | 0.0000001 | \*       |
| Phosphate | 2018 | OW-GS    | 0.0153396 | \*       |
| Phosphate | 2018 | GS       | 0.0011471 | \*       |
| Phosphate | 2020 | OW-GS    | 0.0057749 | \*       |
| Phosphate | 2021 | OW       | 0.0000002 | \*       |

Significant differences between sites

| analyte   | YEAR | Purpose2 |   p.value | asterisk |
|:----------|-----:|:---------|----------:|:---------|
| Ammonium  | 2017 | OW       | 0.0526606 | NA       |
| Ammonium  | 2018 | OW-GS    | 0.2519457 | NA       |
| Ammonium  | 2018 | GS       | 0.0000000 | \*       |
| Ammonium  | 2019 | OW       | 0.0000003 | \*       |
| Ammonium  | 2019 | GS       | 0.0000644 | \*       |
| Ammonium  | 2020 | OW-GS    | 0.7553321 | NA       |
| Ammonium  | 2021 | OW       | 0.8304596 | NA       |
| Nitrate   | 2017 | OW       | 0.0223603 | \*       |
| Nitrate   | 2018 | OW-GS    | 0.0425072 | \*       |
| Nitrate   | 2018 | GS       | 0.0172312 | \*       |
| Nitrate   | 2019 | OW       |       NaN | NA       |
| Nitrate   | 2019 | GS       | 0.0915005 | NA       |
| Nitrate   | 2020 | OW-GS    | 0.0011635 | \*       |
| Nitrate   | 2021 | OW       | 0.0000000 | \*       |
| Phosphate | 2017 | OW       | 0.0000001 | \*       |
| Phosphate | 2018 | OW-GS    | 0.0153396 | \*       |
| Phosphate | 2018 | GS       | 0.0011471 | \*       |
| Phosphate | 2019 | OW       | 0.1746391 | NA       |
| Phosphate | 2019 | GS       |       NaN | NA       |
| Phosphate | 2020 | OW-GS    | 0.0057749 | \*       |
| Phosphate | 2021 | OW       | 0.0000002 | \*       |

differences between sites all

</details>
</details>

## Fertilized plots (Pore water)

<details>
<summary>
click to open
</summary>

#### Pore water data:

pore water measurements from fertilized plots showed little variation in
N components, but significantly more PO4 in 2017 and 2018 all season,
with concentrations returning to that of the other plots near the end of
2019 in xeric and mesic. In Hydric concentrations of PO4 in pore water
were also significantly higher in 2017, and at the beginning of 2018 and
return to similar concentrations of other plots at the end of 2018, and
remained similar for 2019.
<details>
<summary>
click to open LME results
</summary>
<img src="Aggie_Seasonal_files/figure-gfm/unnamed-chunk-9-1.png" width="100%" /><img src="Aggie_Seasonal_files/figure-gfm/unnamed-chunk-9-2.png" width="100%" /><img src="Aggie_Seasonal_files/figure-gfm/unnamed-chunk-9-3.png" width="100%" /><img src="Aggie_Seasonal_files/figure-gfm/unnamed-chunk-9-4.png" width="100%" /><img src="Aggie_Seasonal_files/figure-gfm/unnamed-chunk-9-5.png" width="100%" />
</details>

###### Pore water LME:

<details>
<summary>
click to open LME results
</summary>

| analyte | variable                  | numDF | denDF |    F-value |   p_value | p_value == round(p_value, 3) | asterisk |
|:--------|:--------------------------|------:|------:|-----------:|----------:|:-----------------------------|:---------|
| NH4     | MONTH                     |     1 |  2519 |  23.613547 | 0.0000012 | FALSE                        | \*       |
| NH4     | YEAR                      |     1 |  2519 | 222.858139 | 0.0000000 | TRUE                         | \*       |
| NH4     | MONTH:YEAR                |     1 |  2519 |  26.076260 | 0.0000004 | FALSE                        | \*       |
| NH4     | YEAR:treatment            |     3 |  2519 |  17.072934 | 0.0000000 | FALSE                        | \*       |
| NH4     | MONTH:YEAR:treatment      |     3 |  2519 |   6.656544 | 0.0001785 | FALSE                        | \*       |
| NH4     | YEAR:Site:treatment       |     6 |  2519 |   2.142476 | 0.0457770 | FALSE                        | \*       |
| NH4     | MONTH:YEAR:Site:treatment |     6 |  2519 |   3.822831 | 0.0008478 | FALSE                        | \*       |
| NO3     | YEAR                      |     1 |  2451 |  71.195558 | 0.0000000 | TRUE                         | \*       |
| NO3     | Site                      |     2 |  2451 |  37.262319 | 0.0000000 | FALSE                        | \*       |
| NO3     | treatment                 |     3 |  2451 |   3.078040 | 0.0265207 | FALSE                        | \*       |
| NO3     | MONTH:Site                |     2 |  2451 |  15.651052 | 0.0000002 | FALSE                        | \*       |
| NO3     | MONTH:treatment           |     3 |  2451 |   3.621275 | 0.0126115 | FALSE                        | \*       |
| NO3     | YEAR:treatment            |     3 |  2451 |   8.385542 | 0.0000152 | FALSE                        | \*       |
| NO3     | Site:treatment            |     6 |  2451 |   6.335395 | 0.0000013 | FALSE                        | \*       |
| NO3     | MONTH:YEAR:treatment      |     3 |  2451 |   5.301888 | 0.0012129 | FALSE                        | \*       |
| NO3     | MONTH:Site:treatment      |     6 |  2451 |   4.869738 | 0.0000590 | FALSE                        | \*       |
| NO3     | YEAR:Site:treatment       |     6 |  2451 |   2.188305 | 0.0413914 | FALSE                        | \*       |
| NO3     | MONTH:YEAR:Site:treatment |     6 |  2451 |   7.250415 | 0.0000001 | FALSE                        | \*       |
| PO4     | MONTH                     |     1 |  2399 |  12.714676 | 0.0003699 | FALSE                        | \*       |
| PO4     | YEAR                      |     1 |  2399 |  36.907364 | 0.0000000 | FALSE                        | \*       |
| PO4     | Site                      |     2 |  2399 |   4.591018 | 0.0102318 | FALSE                        | \*       |
| PO4     | treatment                 |     3 |  2399 |  32.581219 | 0.0000000 | TRUE                         | \*       |
| PO4     | MONTH:treatment           |     3 |  2399 |  16.607878 | 0.0000000 | FALSE                        | \*       |
| PO4     | YEAR:treatment            |     3 |  2399 |   7.921750 | 0.0000295 | FALSE                        | \*       |
| PO4     | Site:treatment            |     6 |  2399 |  10.502761 | 0.0000000 | FALSE                        | \*       |
| PO4     | MONTH:YEAR:treatment      |     3 |  2399 |  21.929746 | 0.0000000 | FALSE                        | \*       |
| PO4     | MONTH:Site:treatment      |     6 |  2399 |   3.880157 | 0.0007359 | FALSE                        | \*       |
| PO4     | YEAR:Site:treatment       |     6 |  2399 |   3.614012 | 0.0014277 | FALSE                        | \*       |
| PO4     | MONTH:YEAR:Site:treatment |     6 |  2399 |   8.323263 | 0.0000000 | FALSE                        | \*       |
| TFPA    | MONTH                     |     1 |  2346 |   4.364881 | 0.0367945 | FALSE                        | \*       |
| TFPA    | Site                      |     2 |  2346 |   6.300836 | 0.0018660 | FALSE                        | \*       |
| TFPA    | YEAR:Site                 |     2 |  2346 |   3.253995 | 0.0387940 | FALSE                        | \*       |
| TRS     | MONTH                     |     1 |  2497 |  37.472507 | 0.0000000 | FALSE                        | \*       |
| TRS     | YEAR                      |     1 |  2497 | 128.289731 | 0.0000000 | TRUE                         | \*       |
| TRS     | Site                      |     2 |  2497 |   5.368236 | 0.0047163 | FALSE                        | \*       |
| TRS     | treatment                 |     3 |  2497 |   5.158927 | 0.0014823 | FALSE                        | \*       |
| TRS     | MONTH:YEAR                |     1 |  2497 |  33.647837 | 0.0000000 | FALSE                        | \*       |
| TRS     | YEAR:Site                 |     2 |  2497 |   3.402213 | 0.0334540 | FALSE                        | \*       |
| TRS     | MONTH:treatment           |     3 |  2497 |   6.646371 | 0.0001811 | FALSE                        | \*       |
| TRS     | MONTH:YEAR:Site           |     2 |  2497 |   5.390973 | 0.0046107 | FALSE                        | \*       |

Significant LME comparisons

| analyte | variable                  | numDF | denDF |     F-value |   p_value | p_value == round(p_value, 3) | asterisk |
|:--------|:--------------------------|------:|------:|------------:|----------:|:-----------------------------|:---------|
| Mass    | MONTH                     |     1 |  2486 |   0.0989833 | 0.7530795 | FALSE                        | NA       |
| Mass    | YEAR                      |     1 |  2486 |   1.4667719 | 0.2259708 | FALSE                        | NA       |
| Mass    | Site                      |     2 |  2486 |   1.1171260 | 0.3273831 | FALSE                        | NA       |
| Mass    | treatment                 |     3 |  2486 |   1.1074982 | 0.3447382 | FALSE                        | NA       |
| Mass    | MONTH:YEAR                |     1 |  2486 |   0.0951064 | 0.7578090 | FALSE                        | NA       |
| Mass    | MONTH:Site                |     2 |  2486 |   0.0119858 | 0.9880858 | FALSE                        | NA       |
| Mass    | YEAR:Site                 |     2 |  2486 |   0.9708924 | 0.3788885 | FALSE                        | NA       |
| Mass    | MONTH:treatment           |     3 |  2486 |   0.0014793 | 0.9999215 | FALSE                        | NA       |
| Mass    | YEAR:treatment            |     3 |  2486 |   1.0230944 | 0.3812582 | FALSE                        | NA       |
| Mass    | Site:treatment            |     6 |  2486 |   0.8936561 | 0.4984786 | FALSE                        | NA       |
| Mass    | MONTH:YEAR:Site           |     2 |  2486 |   0.0676554 | 0.9345842 | FALSE                        | NA       |
| Mass    | MONTH:YEAR:treatment      |     3 |  2486 |   0.0158035 | 0.9972924 | FALSE                        | NA       |
| Mass    | MONTH:Site:treatment      |     6 |  2486 |   0.0151719 | 0.9999848 | FALSE                        | NA       |
| Mass    | YEAR:Site:treatment       |     6 |  2486 |   0.7238450 | 0.6303962 | FALSE                        | NA       |
| Mass    | MONTH:YEAR:Site:treatment |     6 |  2486 |   0.0284329 | 0.9999027 | FALSE                        | NA       |
| NH4     | MONTH                     |     1 |  2519 |  23.6135468 | 0.0000012 | FALSE                        | \*       |
| NH4     | YEAR                      |     1 |  2519 | 222.8581386 | 0.0000000 | TRUE                         | \*       |
| NH4     | Site                      |     2 |  2519 |   0.1199802 | 0.8869431 | FALSE                        | NA       |
| NH4     | treatment                 |     3 |  2519 |   0.8359364 | 0.4740167 | FALSE                        | NA       |
| NH4     | MONTH:YEAR                |     1 |  2519 |  26.0762600 | 0.0000004 | FALSE                        | \*       |
| NH4     | MONTH:Site                |     2 |  2519 |   1.1138621 | 0.3284503 | FALSE                        | NA       |
| NH4     | YEAR:Site                 |     2 |  2519 |   0.0851350 | 0.9183909 | FALSE                        | NA       |
| NH4     | MONTH:treatment           |     3 |  2519 |   0.7678538 | 0.5119530 | FALSE                        | NA       |
| NH4     | YEAR:treatment            |     3 |  2519 |  17.0729338 | 0.0000000 | FALSE                        | \*       |
| NH4     | Site:treatment            |     6 |  2519 |   1.3180046 | 0.2453784 | FALSE                        | NA       |
| NH4     | MONTH:YEAR:Site           |     2 |  2519 |   1.8076051 | 0.1642593 | FALSE                        | NA       |
| NH4     | MONTH:YEAR:treatment      |     3 |  2519 |   6.6565440 | 0.0001785 | FALSE                        | \*       |
| NH4     | MONTH:Site:treatment      |     6 |  2519 |   1.3995448 | 0.2109031 | FALSE                        | NA       |
| NH4     | YEAR:Site:treatment       |     6 |  2519 |   2.1424763 | 0.0457770 | FALSE                        | \*       |
| NH4     | MONTH:YEAR:Site:treatment |     6 |  2519 |   3.8228306 | 0.0008478 | FALSE                        | \*       |
| NO3     | MONTH                     |     1 |  2451 |   0.2084546 | 0.6480219 | FALSE                        | NA       |
| NO3     | YEAR                      |     1 |  2451 |  71.1955582 | 0.0000000 | TRUE                         | \*       |
| NO3     | Site                      |     2 |  2451 |  37.2623193 | 0.0000000 | FALSE                        | \*       |
| NO3     | treatment                 |     3 |  2451 |   3.0780403 | 0.0265207 | FALSE                        | \*       |
| NO3     | MONTH:YEAR                |     1 |  2451 |   1.3242788 | 0.2499378 | FALSE                        | NA       |
| NO3     | MONTH:Site                |     2 |  2451 |  15.6510519 | 0.0000002 | FALSE                        | \*       |
| NO3     | YEAR:Site                 |     2 |  2451 |   0.7696518 | 0.4632862 | FALSE                        | NA       |
| NO3     | MONTH:treatment           |     3 |  2451 |   3.6212749 | 0.0126115 | FALSE                        | \*       |
| NO3     | YEAR:treatment            |     3 |  2451 |   8.3855420 | 0.0000152 | FALSE                        | \*       |
| NO3     | Site:treatment            |     6 |  2451 |   6.3353952 | 0.0000013 | FALSE                        | \*       |
| NO3     | MONTH:YEAR:Site           |     2 |  2451 |   1.5630873 | 0.2096971 | FALSE                        | NA       |
| NO3     | MONTH:YEAR:treatment      |     3 |  2451 |   5.3018875 | 0.0012129 | FALSE                        | \*       |
| NO3     | MONTH:Site:treatment      |     6 |  2451 |   4.8697383 | 0.0000590 | FALSE                        | \*       |
| NO3     | YEAR:Site:treatment       |     6 |  2451 |   2.1883048 | 0.0413914 | FALSE                        | \*       |
| NO3     | MONTH:YEAR:Site:treatment |     6 |  2451 |   7.2504147 | 0.0000001 | FALSE                        | \*       |
| PO4     | MONTH                     |     1 |  2399 |  12.7146757 | 0.0003699 | FALSE                        | \*       |
| PO4     | YEAR                      |     1 |  2399 |  36.9073636 | 0.0000000 | FALSE                        | \*       |
| PO4     | Site                      |     2 |  2399 |   4.5910180 | 0.0102318 | FALSE                        | \*       |
| PO4     | treatment                 |     3 |  2399 |  32.5812186 | 0.0000000 | TRUE                         | \*       |
| PO4     | MONTH:YEAR                |     1 |  2399 |   2.0825569 | 0.1491210 | FALSE                        | NA       |
| PO4     | MONTH:Site                |     2 |  2399 |   1.4489132 | 0.2350308 | FALSE                        | NA       |
| PO4     | YEAR:Site                 |     2 |  2399 |   2.9589467 | 0.0520629 | FALSE                        | NA       |
| PO4     | MONTH:treatment           |     3 |  2399 |  16.6078783 | 0.0000000 | FALSE                        | \*       |
| PO4     | YEAR:treatment            |     3 |  2399 |   7.9217500 | 0.0000295 | FALSE                        | \*       |
| PO4     | Site:treatment            |     6 |  2399 |  10.5027613 | 0.0000000 | FALSE                        | \*       |
| PO4     | MONTH:YEAR:Site           |     2 |  2399 |   1.2369229 | 0.2904611 | FALSE                        | NA       |
| PO4     | MONTH:YEAR:treatment      |     3 |  2399 |  21.9297457 | 0.0000000 | FALSE                        | \*       |
| PO4     | MONTH:Site:treatment      |     6 |  2399 |   3.8801569 | 0.0007359 | FALSE                        | \*       |
| PO4     | YEAR:Site:treatment       |     6 |  2399 |   3.6140115 | 0.0014277 | FALSE                        | \*       |
| PO4     | MONTH:YEAR:Site:treatment |     6 |  2399 |   8.3232635 | 0.0000000 | FALSE                        | \*       |
| TFPA    | MONTH                     |     1 |  2346 |   4.3648813 | 0.0367945 | FALSE                        | \*       |
| TFPA    | YEAR                      |     1 |  2346 |   0.0253375 | 0.8735426 | FALSE                        | NA       |
| TFPA    | Site                      |     2 |  2346 |   6.3008362 | 0.0018660 | FALSE                        | \*       |
| TFPA    | treatment                 |     3 |  2346 |   1.0004341 | 0.3916216 | FALSE                        | NA       |
| TFPA    | MONTH:YEAR                |     1 |  2346 |   1.0569400 | 0.3040201 | FALSE                        | NA       |
| TFPA    | MONTH:Site                |     2 |  2346 |   0.3083943 | 0.7346554 | FALSE                        | NA       |
| TFPA    | YEAR:Site                 |     2 |  2346 |   3.2539953 | 0.0387940 | FALSE                        | \*       |
| TFPA    | MONTH:treatment           |     3 |  2346 |   2.5431356 | 0.0545869 | FALSE                        | NA       |
| TFPA    | YEAR:treatment            |     3 |  2346 |   0.5679299 | 0.6361483 | FALSE                        | NA       |
| TFPA    | Site:treatment            |     6 |  2346 |   1.3488158 | 0.2318808 | FALSE                        | NA       |
| TFPA    | MONTH:YEAR:Site           |     2 |  2346 |   0.0586504 | 0.9430378 | FALSE                        | NA       |
| TFPA    | MONTH:YEAR:treatment      |     3 |  2346 |   0.5373310 | 0.6567211 | FALSE                        | NA       |
| TFPA    | MONTH:Site:treatment      |     6 |  2346 |   1.0132753 | 0.4146243 | FALSE                        | NA       |
| TFPA    | YEAR:Site:treatment       |     6 |  2346 |   0.6132802 | 0.7198969 | FALSE                        | NA       |
| TFPA    | MONTH:YEAR:Site:treatment |     6 |  2346 |   0.2849393 | 0.9443101 | FALSE                        | NA       |
| TRS     | MONTH                     |     1 |  2497 |  37.4725075 | 0.0000000 | FALSE                        | \*       |
| TRS     | YEAR                      |     1 |  2497 | 128.2897311 | 0.0000000 | TRUE                         | \*       |
| TRS     | Site                      |     2 |  2497 |   5.3682356 | 0.0047163 | FALSE                        | \*       |
| TRS     | treatment                 |     3 |  2497 |   5.1589272 | 0.0014823 | FALSE                        | \*       |
| TRS     | MONTH:YEAR                |     1 |  2497 |  33.6478369 | 0.0000000 | FALSE                        | \*       |
| TRS     | MONTH:Site                |     2 |  2497 |   2.4135020 | 0.0897101 | FALSE                        | NA       |
| TRS     | YEAR:Site                 |     2 |  2497 |   3.4022125 | 0.0334540 | FALSE                        | \*       |
| TRS     | MONTH:treatment           |     3 |  2497 |   6.6463707 | 0.0001811 | FALSE                        | \*       |
| TRS     | YEAR:treatment            |     3 |  2497 |   0.4093911 | 0.7462688 | FALSE                        | NA       |
| TRS     | Site:treatment            |     6 |  2497 |   0.2455244 | 0.9612100 | FALSE                        | NA       |
| TRS     | MONTH:YEAR:Site           |     2 |  2497 |   5.3909732 | 0.0046107 | FALSE                        | \*       |
| TRS     | MONTH:YEAR:treatment      |     3 |  2497 |   0.4793736 | 0.6966532 | FALSE                        | NA       |
| TRS     | MONTH:Site:treatment      |     6 |  2497 |   0.4554882 | 0.8414601 | FALSE                        | NA       |
| TRS     | YEAR:Site:treatment       |     6 |  2497 |   0.5914919 | 0.7374033 | FALSE                        | NA       |
| TRS     | MONTH:YEAR:Site:treatment |     6 |  2497 |   0.5389044 | 0.7789662 | FALSE                        | NA       |

All LME comparisons

</details>
</details>

## FTICR

<details>
<summary>
click to open
</summary>

#### PCA by Site:

FTICR revealed large differences in organic matter content based on
site, and small variation based on time of year. Mesic contains far more
aromatic, condensed aromatic, and unsaturated lignin compounds (Note
that it does not have the highest concentrations of phenolics). Site
explained ~70% of the variation in FTICR compound diversity, where as
the interaction between Site:year explained ~10% of the variation.

<details>
<summary>
click to open
</summary>

<img src="Aggie_Seasonal_files/figure-gfm/unnamed-chunk-11-1.png" width="100%" /><img src="Aggie_Seasonal_files/figure-gfm/unnamed-chunk-11-2.png" width="100%" />

</details>

###### PERMANOVA:

<details>
<summary>
click to open
</summary>

|             |  Df | SumsOfSqs |   MeanSqs |     F.Model |        R2 | Pr(\>F) |
|:------------|----:|----------:|----------:|------------:|----------:|--------:|
| Site        |   2 | 0.2048378 | 0.1024189 | 109.2342208 | 0.7080634 |   0.001 |
| Year        |   1 | 0.0011243 | 0.0011243 |   1.1990670 | 0.0038862 |   0.274 |
| Season      |   2 | 0.0013712 | 0.0006856 |   0.7312319 | 0.0047399 |   0.493 |
| Site:Year   |   2 | 0.0278093 | 0.0139047 |  14.8299373 | 0.0961286 |   0.001 |
| Site:Season |   3 | 0.0083759 | 0.0027920 |   2.9777402 | 0.0289529 |   0.034 |
| Year:Season |   1 | 0.0007694 | 0.0007694 |   0.8205613 | 0.0026595 |   0.388 |
| Residuals   |  48 | 0.0450052 | 0.0009376 |          NA | 0.1555696 |      NA |
| Total       |  59 | 0.2892930 |        NA |          NA | 1.0000000 |      NA |

Polar PERMANOVA results

|             |  Df |  SumsOfSqs |    MeanSqs |    F.Model |         R2 | Pr(\>F) |
|:------------|----:|-----------:|-----------:|-----------:|-----------:|--------:|
| Site        |   2 |  0.0815983 |  0.0407992 | 95.7669139 |  0.6687323 |   0.001 |
| Year        |   1 |  0.0031369 |  0.0031369 |  7.3631567 |  0.0257082 |   0.016 |
| Season      |   2 |  0.0019339 |  0.0009670 |  2.2697396 |  0.0158494 |   0.121 |
| Site:Year   |   2 |  0.0123377 |  0.0061689 | 14.4800301 |  0.1011128 |   0.001 |
| Site:Season |   3 |  0.0025707 |  0.0008569 |  2.0113723 |  0.0210679 |   0.120 |
| Year:Season |   1 | -0.0000074 | -0.0000074 | -0.0173272 | -0.0000605 |   0.999 |
| Residuals   |  48 |  0.0204492 |  0.0004260 |         NA |  0.1675900 |      NA |
| Total       |  59 |  0.1220194 |         NA |         NA |  1.0000000 |      NA |

Non-Polar PERMANOVA results

In polar and nonpolar sample extracts Site, Site:Year were significant
(p \< 0.05)

Site accounted for ~70 % of total variation among samples Site:Year
accounted for ~10% of total variation among samples

</details>
</details>

## Session Info

<details>
<summary>
Session Info
</summary>

Date run: 2023-09-13

    ## R version 4.2.3 (2023-03-15 ucrt)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 19045)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United States.utf8 
    ## [2] LC_CTYPE=English_United States.utf8   
    ## [3] LC_MONETARY=English_United States.utf8
    ## [4] LC_NUMERIC=C                          
    ## [5] LC_TIME=English_United States.utf8    
    ## 
    ## attached base packages:
    ## [1] grid      stats     graphics  grDevices utils     datasets  methods  
    ## [8] base     
    ## 
    ## other attached packages:
    ##  [1] cowplot_1.1.1     agricolae_1.3-6   doBy_4.6.17       ggpubr_0.6.0     
    ##  [5] pracma_2.4.2      reshape2_1.4.4    ggbreak_0.1.2     ggExtra_0.10.0   
    ##  [9] lubridate_1.9.2   forcats_1.0.0     stringr_1.5.0     dplyr_1.1.2      
    ## [13] purrr_1.0.1       readr_2.1.4       tidyr_1.3.0       tibble_3.2.1     
    ## [17] tidyverse_2.0.0   ggbiplot_0.55     scales_1.2.1      plyr_1.8.8       
    ## [21] ggplot2_3.4.1     vegan_2.6-4       lattice_0.20-45   permute_0.9-7    
    ## [25] tarchetypes_0.7.7 targets_1.2.0    
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] colorspace_2.1-0      ggsignif_0.6.4        ellipsis_0.3.2       
    ##  [4] fs_1.6.2              aplot_0.2.0           rstudioapi_0.15.0    
    ##  [7] farver_2.1.1          listenv_0.9.0         furrr_0.3.1          
    ## [10] Deriv_4.1.3           fansi_1.0.4           codetools_0.2-19     
    ## [13] splines_4.2.3         cachem_1.0.8          knitr_1.43           
    ## [16] broom_1.0.5           cluster_2.1.4         shiny_1.7.4.1        
    ## [19] compiler_4.2.3        backports_1.4.1       Matrix_1.6-0         
    ## [22] fastmap_1.1.1         cli_3.6.1             later_1.3.1          
    ## [25] htmltools_0.5.5       tools_4.2.3           igraph_1.5.0         
    ## [28] gtable_0.3.3          glue_1.6.2            Rcpp_1.0.11          
    ## [31] carData_3.0-5         vctrs_0.6.3           nlme_3.1-162         
    ## [34] xfun_0.39             globals_0.16.2        ps_1.7.5             
    ## [37] timechange_0.2.0      mime_0.12             miniUI_0.1.1.1       
    ## [40] lifecycle_1.0.3       rstatix_0.7.2         future_1.33.0        
    ## [43] MASS_7.3-58.2         microbenchmark_1.4.10 hms_1.1.3            
    ## [46] promises_1.2.0.1      parallel_4.2.3        yaml_2.3.7           
    ## [49] memoise_2.0.1         ggfun_0.1.2           yulab.utils_0.0.7    
    ## [52] labelled_2.12.0       stringi_1.7.12        highr_0.10           
    ## [55] klaR_1.7-2            AlgDesign_1.2.1       PNWColors_0.1.0      
    ## [58] rlang_1.1.1           pkgconfig_2.0.3       evaluate_0.21        
    ## [61] labeling_0.4.2        patchwork_1.1.2       processx_3.8.2       
    ## [64] tidyselect_1.2.0      parallelly_1.36.0     magrittr_2.0.3       
    ## [67] R6_2.5.1              generics_0.1.3        base64url_1.4        
    ## [70] combinat_0.0-8        pillar_1.9.0          haven_2.5.3          
    ## [73] withr_2.5.0           mgcv_1.8-42           abind_1.4-5          
    ## [76] car_3.1-2             questionr_0.7.8       utf8_1.2.3           
    ## [79] rmarkdown_2.23        tzdb_0.4.0            future.callr_0.8.1   
    ## [82] data.table_1.14.8     callr_3.7.3           digest_0.6.33        
    ## [85] xtable_1.8-4          httpuv_1.6.11         gridGraphics_0.5-1   
    ## [88] munsell_0.5.0         ggplotify_0.1.2

</details>