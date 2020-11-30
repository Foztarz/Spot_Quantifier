# Spot_Quantifier
An R script for quantifying "Spot Assays", in which rectangular plates are pinned with media of different concentrations in a rectangular grid. Quantifies bacterial growth as the proportion of a grid-space rectangle covered by bacterial colonies (as a number between 0 and 1).
Input is a user-selected folder containing "```.JPG```" images of plates. For each plate, ```Spot_QuantLoop.R``` outputs the proportions all grid-spaces as an Excel-compatible "```.csv```" file, and plots showing the grid fitted and the colonies found for each grid-space as "```.jpeg```" files, and  [contours](https://en.wikipedia.org/wiki/Contour_line) across the plate as "```.png```" files.
All plots are optional: the user can set each type to ```TRUE``` or ```FALSE``` at line 50,
```r
plot_these <- list(grid = TRUE, #Fitted grid
                   found = TRUE,  #Colonies found
                   contour = TRUE #Contour plot
                   )
 ```
 .
## Example output:
![Fitted grid](https://github.com/Foztarz/Spot_Quantifier/blob/master/ExampleOutput/CB-Killing_01h_11-07-20.JPG-Grid.jpeg)
![Colonies found](https://github.com/Foztarz/Spot_Quantifier/blob/master/ExampleOutput/CB-Killing_01h_11-07-20.JPG-GridCB-Killing_01h_11-07-20.JPG-OtsuFound.jpeg)
![Contour plot](https://github.com/Foztarz/Spot_Quantifier/blob/master/ExampleOutput/CB-Killing_01h_11-07-20.JPG-Contour.png)
###Results
|       |column 1	|column 2	|column 3	|column 4	|column 5	|column 6	|column 7	|column 8	|column 9	|column 10	|column 11	|column 12 |
| ----- | ------- | ------- | ------- | ------- | ------- | ------- | ------- | ------- | ------- | ------- | ------- | ------- |
row 1	0.29754536	0.000108155	0.000734315	0.000711545	0	9.49E-06	0	0	0.000466774	9.68E-05	0.000130924	0.000396808
row 2	0.296545222	0.002938125	0.00032751	0.000261251	0.000200671	0	0	0.000113587	4.92E-05	0.000157129	0	2.46E-05
row 3	0.385395524	7.37E-05	2.30E-06	7.14E-05	1.38E-05	7.83E-05	0	0.000181961	2.30E-05	0	0	7.49E-06
row 4	0.346894598	2.00E-06	6.81E-05	0	0.000178255	0.000194278	0	0	0	7.81E-05	1.20E-05	2.60E-05
row 5	0.347398119	0.000235473	2.42E-05	4.83E-05	4.83E-05	7.45E-05	0.000199246	0.000573588	7.45E-05	0	0.000191196	0
row 6	0.333874601	0.000411748	0.000561646	3.61E-05	0.000204925	5.50E-05	2.85E-05	0.00030549	0	0	5.50E-05	9.05E-05
row 7	0.369491512	0.007088739	0.002715182	0.000607664	0.000317042	7.32E-05	0	2.44E-05	0.000276396	0.000126004	5.49E-05	0
row 8	0.483966584	0.488997797	0.012249697	0.000690577	0.00012871	0.000183164	0	0.0006683	0.000249994	0.000482661	0.000279696	0.000136782
