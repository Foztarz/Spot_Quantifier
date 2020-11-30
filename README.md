# Spot_Quantifier
An R script for quantifying "Spot Assays", in which rectangular plates are pinned with media of different concentrations in a rectangular grid. Quantifies bacterial growth as the proportion of a grid-space rectangle covered by bacterial colonies (as a number between 0 and 1).
Input is a user-selected folder containing "```.JPG```" images of plates. For each plate, ```Spot_QuantLoop.R``` outputs the proportions all grid-spaces as an Excel-compatible "```.csv```" file, and plots showing the grid fitted and the thresholded colonies for each grid-space as "```.jpeg```" files, and  [contours](https://en.wikipedia.org/wiki/Contour_line) across the plate as "```.png```" files.
All plots are optional: the user can set each type to ```TRUE``` or ```FALSE``` at line 50,
```{r}
plot_these <- list(grid = TRUE,
                   found = TRUE,
                   contour = TRUE
                   )
 ```
