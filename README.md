# Spot_Quantifier
An R script for quantifying "Spot Assays", in which rectangular plates are pinned with media of different concentrations in a rectangular grid. Quantifies bacterial growth as the proportion of a grid-space rectangle covered by bacterial colonies (as a number between 0 and 1).
Input is a user-selected folder containing "```.JPG```" images of plates. For each plate, ```Spot_QuantLoop.R``` outputs the proportions all grid-spaces as an Excel-compatible "```.csv```" file, and plots showing the grid fitted and the colonies found for each grid-space as "```.jpeg```" files, and  [contours](https://en.wikipedia.org/wiki/Contour_line) across the plate as "```.png```" files.
All plots are optional: the user can set each type to ```TRUE``` or ```FALSE``` at line 50.
```r
plot_these <- list(grid = TRUE, #Fitted grid
                   found = TRUE,  #Colonies found
                   contour = TRUE #Contour plot
                   )
 ```
## Update 2021.07.13
The user-selected parameter ```is_robot``` now allows the user to specify that the plate was spotted in an automatic grid, which should not vary in spacing. If set to ```TRUE```, this makes the search for the plate's edge less flexible, and draws an evenly-spaced rigid grid across that plate area (by setting ```spc_lim``` to ```0```) regardless of actual colony positions. This setting is still under development at present, other values for user-selected parameters may be recommended or bypassed completely in the near future after further testing.
## Example output:
![Fitted grid](https://github.com/Foztarz/Spot_Quantifier/blob/master/ExampleOutput/CB-Killing_01h_11-07-20.JPG-Grid.jpeg)
![Colonies found](https://github.com/Foztarz/Spot_Quantifier/blob/master/ExampleOutput/CB-Killing_01h_11-07-20.JPG-OtsuFound.jpeg)
![Contour plot](https://github.com/Foztarz/Spot_Quantifier/blob/master/ExampleOutput/CB-Killing_01h_11-07-20.JPG-Contour.png)
### Results
|       |column 1	|column 2	|column 3	|column 4	|column 5	|column 6	|column 7	|column 8	|column 9	|column 10	|column 11	|column 12 |
| ----- | ------- | ------- | ------- | ------- | ------- | ------- | ------- | ------- | ------- | ------- | ------- | ------- |
| **row 1** |	0.2975  | 0.0001  |	0.0007  |	0.0007  |	0	9.49E-06 |  	0    |	  0    |	0.0004  |	9.68E-05  |	0.0001  |	0.0003  |
| **row 2** |	0.2965  |	0.0029  |	0.0003  |	0.0002  |	0.0002  |	  0    |	  0  |	0.0001  |	4.92E-05  |	0.0001  |	  0    |	2.46E-05  |
| **row 3** |	0.3853  |	7.37E-05  |	2.30E-06  |	7.14E-05  |	1.38E-05  |	7.83E-05  |	0  |	0.0001  |	2.30E-05  |	0  |	0  |	7.49E-06  |
| **row 4** |	0.3468  |	2.00E-06  |	6.81E-05  |	0 |		0.0001  |	0.0001  |	0 |		0 |		0	 |	7.81E-05  |	1.20E-05  |	2.60E-05  |
| **row 5** |	0.3473  |	0.0002  |	2.42E-05  |	4.83E-05  |	4.83E-05  |	7.45E-05  |	0.0001  |	0.0005  |	7.45E-05  |	0  |	0.000191196  |	0  |
| **row 6** |	0.3338  |	0.0004  |	0.0005  |	3.61E-05  |	0.0002  |	5.50E-05  |	2.85E-05  |	0.0003  |	0 |		0	 |	5.50E-05  |	9.05E-05  |
| **row 7** |	0.3694  | 0.0071  |	0.0027  |	0.0006  |	0.0003  |	7.32E-05  |	0 |		2.44E-05  |	0.0002  |	0.0001  |	5.49E-05  |	0  |
| **row 8** |	0.4839  |	0.4889  |	0.0122  |	0.0006  |	0.0001  |	0.0001  |	0 |		0.0006  |	0.0002  |	0.0004  |	0.0002  |	0.0001  |
