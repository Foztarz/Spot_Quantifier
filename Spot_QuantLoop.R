# rm(list = ls()) #FOR A 'CLEAN' RUN, RESTART Rstudio: rm(list = ls()) is unfashionable
graphics.off()
#R versions <4.0.0 convert strings to factors, specify default behaviour
formals(data.frame)$stringsAsFactors <- FALSE
# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster              DATE: 2020 11 10
#     MODIFIED:	James Foster              DATE: 2021 07 13
#
#  DESCRIPTION: Loops through one spot assay images. Crops and identifies grid.
#               Automatically fits grid lines so that they intersect with as few
#               colonies as possible. Measures opacity within each grid rectangle, 
#               as proportion of grid rectangle containing colonies.
#               
#               
#               
#      OUTPUTS: Plot as PNG and data as CSV.
#
#	   CHANGES: -copied from Spot_Quantifier.R
#	            -corner double-counts cut
#	            -1536 grid
#	            -rigid grid option
#
#   REFERENCES: Kritikos, G., Banzhaf, M., Herrera-Dominguez, L. et al., (2017).
#               A tool named Iris for versatile high-throughput phenotyping in microorganisms.
#               Nature microbiology, 2(5), 1-10.
#               https://doi.org/10.1038/nmicrobiol.2017.14
#
#    EXAMPLES:  
#
# 
#TODO                         ---------------------------------------------
#TODO   
#- Folder selector +
#- Looping +
#- Autocropping + 
#- Rigidity setting +
#- Adjust crop  +
#- Fix rigid grid SE +
#- NON-OTSU THRESHOLDING FOR ROBOT
#- Rigid crop setting ?
#- Speed up 
#- imager?

#	User input                  ---------------------------------------------

#Grid rows and columns
grid_pos <- list(
                  columns = 48,  #12,
                  rows = 32  #8
                )
#Who made the plate? (Human or robot)
is_robot <- TRUE#N.B. this predetermines several other parameters 
# Colour channel to use
col_chan <- 'green'#channel to use, 'red','green','blue', combinations ('redgreen') or all (RGB)
# Palette for contour plots #can be any of hcl.pals()
cont_pal <- 'Magenta'
# Which plots are required
plot_these <- list(grid = T,
                   found = T,
                   contour = T
                   )
#maximum cropped proportion
crop_max <- ifelse(is_robot, yes = 0.093, no = 0.10) 
#minimum cropped proportion
crop_min <- ifelse(is_robot, yes = 0.07, no = 0.00) 
#Starting grid edge as a proportion of image height or width
edge_prop <- ifelse(is_robot, yes = 0.08, no = 0.09)
#maximum radio of grid spacing to default, e.g. 0.5 -> rectangles can be max 50% bigger or smaller
spc_lim <- ifelse(is_robot, yes = 0.00, no = 0.3)#0 = no grid search
# Image levels
im_levels <- 256L #Resolution is probably higher, higher values could solve thresholding problems


#check the operating system and assign a logical flag (TRUE or FALSE)
sys_win <- Sys.info()[['sysname']] == 'Windows'
#On computers set up by JMU Würzburg, use user profile instead of home directory
if(sys_win){
  #get rid of all the backslashes
  user_path <- gsub('\\\\', '/', Sys.getenv('USERPROFILE'))#Why does windows have to make this so difficult
}else{#Root directory should be the "HOME" directory on a Mac (or Linux?)
  user_path <- Sys.getenv('HOME')#Life was easier on Mac
}
if(sys_win){#choose.files is only available on Windows
  message('\n\nPlease select the folder containing images \n\n')
  Sys.sleep(0.5)#goes too fast for the user to see the message on some computers
  img_path  <- choose.dir(
    file.path(user_path,'Documents',#suggest a location
              paste0('Spot-assay')
              )
  )
}else{
  message('\n\nPlease select any image in the correct folder\n\n')
  Sys.sleep(0.5)#goes too fast for the user to see the message on some computers
  img_path <- dirname(file.choose(new=F))
}
print(img_path)

# Useful functions --------------------------------------------------------
require(jpeg)

# Formerly in EBImage::otsu, according to https://stackoverflow.com/a/51117372/3745353
otsu <- function(img,  # assuming img is a numeric matrix or vector
                 rng = c(0, 1),   # assuming values in the matrix range from 0 to 1
                 levels = 256L #TODO set this as a user option, res can be higher
)
{
  h <- hist.default(img,
                    breaks = seq(rng[1], rng[2], length.out = levels + 1),
                    plot = FALSE)
  counts <- as.double(h$counts)
  mids <- as.double(h$mids)
  len <- length(counts)
  w1 <- cumsum(counts)
  w2 <- w1[len] + counts - w1
  cm <- counts * mids
  m1 <- cumsum(cm)
  m2 <- m1[len] + cm - m1
  var <- w1 * w2 * (m2/w2 - m1/w1)^2
  maxi <- which(var == max(var, na.rm = TRUE))
  return(
    (mids[maxi[1]] + mids[maxi[length(maxi)]])/2
  )
}

# Dervied variables -------------------------------------------------------
# channel indices
use_channel <- switch(col_chan,#channel to use, 'red','green','blue', or all (RGB)
                      red = 1,
                      green = 2,
                      blue = 3,
                      rgb = 1:3,#RGB is slower
                      RGB = 1:3,
                      greenblue = 2:3,
                      redgreen = 1:2
)
# suggest edge position
grid_edges_prop <- list(v = c(edge_prop, 1-edge_prop), h = c(edge_prop, 1-edge_prop))
# 
# contour_cols <- rev(hcl.colors(26,palette = cont_pal))#use palette instead

if(!file.exists(#if there isn't already a folder called "Out"...
  file.path(img_path, 'Out')
)
){#...make a folder called out
  dir.create(file.path(img_path, 'Out'))
}

# Find images in folder ---------------------------------------------------
imgs <- dir(img_path,pattern = '.JPG$|.jpeg$')#files ending in .JPG or .jpeg

# Read images -------------------------------------------------------------
for(ii in imgs){
  message('\n\nImage ', which(imgs %in% ii), ' of ',length(imgs),'\n\n')
  message('', ii, '\n\n')
  img.jpg <- readJPEG(file.path(img_path, ii))
  grid_edges <- list(v = round(dim(img.jpg)[2]*grid_edges_prop$v),
                     h = round(dim(img.jpg)[1]*grid_edges_prop$h)
  )
  
  # Automated crop boundaries ---------------------------------------------
  # Basic row grid
  r_max <- dim(img.jpg)[1]#outer edge
  # Basic column grid
  c_max <- dim(img.jpg)[2]#outer edge
  
  img.chan <- img.jpg[,,use_channel]
  # img.chan[img.chan>otsu(img.chan)] <- 1
  # img.chan[img.chan<otsu(img.chan)] <- 0
  #rectangle function
  rect_crop <- function(pr){
    # disqualifications
    if(
      any(pr<1) |
      pr[1]>r_max*crop_max |
      pr[2]<r_max*(1-crop_max) |
      pr[2]>r_max |
      pr[3]>c_max*crop_max |
      pr[4]<c_max*(1-crop_max) |
      pr[4]>c_max |
      pr[1]<r_max*crop_min |
      pr[2]>r_max*(1-crop_min) |
      pr[3]<c_max*crop_min |
      pr[4]>c_max*(1-crop_min) 
    )
      {rect_mn <- 1e19}else
      {
        rect_mn <- mean(
                    c(
                      img.chan[pr[1]:pr[2], pr[3]],
                      img.chan[pr[1]:pr[2], pr[4]],
                      img.chan[pr[1], (pr[3]+1):(pr[4]-1)],#avoid double-counting corners
                      img.chan[pr[2], (pr[3]+1):(pr[4]-1)]#avoid double-counting corners
                      )
                  )
      }
    return(rect_mn)
  }
  if(  rect_crop(c(grid_edges$h, grid_edges$v)) > 1e18 )
    {warning('bad start for cropping frame')}
  rect_opt <- optim(c(grid_edges$h, grid_edges$v),
                    rect_crop#,
                    # control = list(trace = T)
                    )
  # plot(as.raster(img.jpg), interpolate = T)
  # abline(h = grid_edges$h, v = grid_edges$v, col = 'cyan')
  # abline(h = dim(img.jpg)[1]-rect_opt$par[1:2], v = rect_opt$par[3:4], col = 'red')
  
  grid_edges$h <- round(rect_opt$par[1:2])
  grid_edges$v <- round(rect_opt$par[3:4])

  # Crop --------------------------------------------------------------------
  img.crp <- img.jpg[grid_edges$h[1]:grid_edges$h[2],
                     grid_edges$v[1]:grid_edges$v[2],
  ]
  
  
  # Automated grid guessing game --------------------------------------------
  # Choose thresholding level
  ots <- otsu(img.crp, levels = im_levels)
  # Basic row grid
  row_max <- diff(grid_edges$h)#outer edge
  row_step <- row_max/grid_pos$rows#size of equidistant steps
  basic_row <- round((1:(grid_pos$rows-1)) * row_step)
  # Basic column grid
  col_max <- diff(grid_edges$v)#outer edge
  col_step <- col_max/grid_pos$columns#size of equidistant steps
  basic_col <- round((1:(grid_pos$columns-1)) * col_step )
  
  # Threshold image
  img.ots <- img.crp[,,use_channel]#use only relevant colour channel(s)
  img.ots[img.ots > otsu(img.crp)] <- 1 #Above threshold values set to maximum
  img.ots[img.ots < otsu(img.crp)] <- 0 #Below threshold values set to minimum
  if(length(use_channel) > 1)
  {#when there are multiple colour channels, sum them
    img.ots <- apply(img.ots, 1:2, sum) #Sum across colour channels #warning SLOW!
  }else{
  }
  # add up colony pixels for each possible grid boundary position
  i_cols <- apply(img.ots, 2, sum)#colony pixels along each column
  i_rows <- apply(img.ots, 1, sum)#colony pixels along each row
  
  # Test out positioning for different equidistant grids
  row_starts <- -(round(row_step/2)):round(row_step/2)#+- half of one grid square
  col_starts <- -(round(col_step/2)):round(col_step/2)#+- half of one grid square
  if( !all( row_starts + round(row_step/2) < max(grid_edges$v) )  |
      !all( col_starts + round(col_step/2) < max(grid_edges$h) )  
  ){warning('specified grid space too small')}
  row_bright <- sapply(row_starts, function(x){sum(i_rows[x+basic_row])})#check the number of colony pixels for each position
  col_bright <- sapply(col_starts, function(x){sum(i_cols[x+basic_col])})#check the number of colony pixels for each position
  # Position the equidistant grid at the midpoint of the best options
  if(!is_robot)
  {
  row_equi <- round(median(row_starts[row_bright %in% min(row_bright,na.rm=T)]))+basic_row
  col_equi <- round(median(col_starts[col_bright %in% min(col_bright,na.rm=T)]))+basic_col
  }else
  {
  row_equi <- basic_row
  col_equi <- basic_col
  }
  if(spc_lim)
  {
  #Set up a function to optimise grid positioning
  g_row_fun <- function(pr){
    if(all(pr > 0 & pr < row_max)){
      #add variance along each line
      px_var <- sum(i_rows[pr])
      #Add outside values
      pr_row <- c(1, pr, row_max)
      # Spacing should be relatively even, also penalise uneven spacing
      sp_row <- diff(pr_row)#scale to pixel variance level
      # strongly penalise deviation from even spacing
      px_var <- ifelse(max(abs(sp_row-row_step)/row_step)>spc_lim, 1e19, px_var)
    }else{px_var <- 1e19}
    return(px_var)
  }
  
  #test the function
  row_try <- 0
  while(g_row_fun(row_equi) >1e16 & row_try <100)#can get stuck at a bad start, add random offset in that case
  {
    row_equi <- basic_row + sample(abs(row_starts), length(row_equi), replace = T)
    row_try <- row_try + 1
    if(row_try >99){warning('Could not find a good starting row grid for\n', ii)}
  }
  grd_row_par <- optim(row_equi, 
                       g_row_fun 
  )
  
  g_col_fun <- function(pr){
    if(all(pr > 0 & pr < col_max)){
      #add variance along each line
      px_var <- sum(i_cols[pr])
      #Add outside values
      pr_col <- c(1, pr, col_max)
      # Spacing should be relatively even, also penalise uneven spacing
      sp_col <- diff(pr_col)#scale to pixel variance level
      # strongly penalise deviation from even spacing
      px_var <- ifelse(max(abs(sp_col-col_step)/col_step)>spc_lim, 1e19, px_var)
    }else{px_var <- 1e19}
    return(px_var)
  }
  
  #test the function
  col_try <- 0
  while(g_col_fun(col_equi) >1e16 & col_try <100)#can get stuck at a bad start, add random offset in that case
  {
    col_equi <- basic_col + sample(abs(col_starts), length(col_equi), replace = T)
    col_try <- col_try + 1
    if(col_try >99){warning('Could not find a good starting column grid for\n', ii)}
  }
  g_col_fun(col_equi)#starting value 0.01860691
  grd_col_par <- optim(col_equi, 
                       g_col_fun 
  )
  }else
  {#for the robot (spc_lim == 0), use a rigid grid
    grd_row_par <- list(par = row_equi)
    grd_col_par <- list(par = col_equi)
  }

  # Plot fitted grid --------------------------------------------------------
  if(plot_these$grid){
    jpeg(filename = file.path(img_path,'Out', paste0(ii, '-Grid.jpeg')),
         height = 4*ifelse(test = all(grid_pos<20),
                           yes = 1, 
                           no = 2
                           ), 
         width = 5*ifelse(test = all(grid_pos<20),
                          yes = 1, 
                          no = 2
                           ),  
         units = 'in', bg = 'white',
         res = ifelse(test = all(grid_pos<15),
                      yes = 150, 
                      no = 300
                      )
    )
    plot(as.raster(img.jpg), interpolate = T)
    abline(h = dim(img.jpg)[1] - (grid_edges$h[1] +grd_row_par$par),
           v = grid_edges$v[1] + grd_col_par$par,
           col = 'lightseagreen',
           lty = 3
           )
    lines(x = c(rep(grid_edges$v[1], row_max+1),
                grid_edges$v[1]:grid_edges$v[2],
                rep(grid_edges$v[2], row_max+1),
                grid_edges$v[2]:grid_edges$v[1]
                ),
          y = dim(img.jpg)[1] - 
                c(grid_edges$h[1]:grid_edges$h[2],
                 rep(grid_edges$h[2], col_max+1),
                 grid_edges$h[2]:grid_edges$h[1],
                 rep(grid_edges$h[1], col_max+1)
                ),
         col = 'lightseagreen',
         lty = 2
          )
    dev.off()
  }  #if(plot_these$grid)
  
  # Loop through and calculate opacity --------------------------------------
    row.breaks <- c(1, grd_row_par$par, row_max)
  col.breaks <- c(1, grd_col_par$par, col_max)
  opmat <- matrix(nrow = grid_pos$rows, ncol = grid_pos$columns,
                  dimnames = list(row = paste('row',1:grid_pos$rows),
                                  column = paste('column', 1:grid_pos$columns)
                  )
  )
  if(plot_these$found){
    # png(filename = file.path(img_path,'Out', paste0(ii, '-OtsuFound.png')),
    #     height = 4, width = 5, units = 'in', bg = 'white', res = 150)
    jpeg(filename = file.path(img_path,'Out', paste0(ii, '-OtsuFound.jpeg')),
         height = 4*ifelse(test = all(grid_pos<20),
                           yes = 1, 
                           no = 2
         ), 
         width = 5*ifelse(test = all(grid_pos<20),
                          yes = 1, 
                          no = 2
         ),  
         units = 'in', bg = 'white',
         res = ifelse(test = all(grid_pos<15),
                      yes = 150, 
                      no = 300
                     )
    )
  }#if(plot_these$found)
  par(mfrow = c(grid_pos$rows,grid_pos$columns), mar = c(0,0,0.8,0), cex.main = 0.8)
  for(rw in 1:grid_pos$rows){
    for(cl in 1:grid_pos$columns){
      img.tmp <- img.crp[
        min( row.breaks[rw], dim(img.crp)[1] ):
          min( c(row.breaks[rw+1], dim(img.crp)[1]) ),#think I have? TODO consider excluding tile edges (row.breaks[rw]+1):(row.breaks[rw+1]-1)
        min( col.breaks[cl],dim(img.crp)[2] ):
          min( c(col.breaks[cl+1], dim(img.crp)[2]) ),#think I have? TODO consider excluding tile edges
        
      ]
      if(plot_these$found){plot(as.raster(img.tmp))}
      img.tmp[img.tmp > ots] <- 1
      if(plot_these$found){
        plot(as.raster(img.tmp), add = T)
        polygon(c(0,0,rep(dim(img.tmp)[2],2)),
                c(0,rep(dim(img.tmp)[1],2),0),
                col = NA,
                border = 'cyan')
      }#if(plot_these$found)
      opmat[rw,cl] <- mean(img.tmp > ots)
      if(plot_these$found){ title(main = paste(signif(opmat[rw,cl],2))) }
    }
  }
  # save PNG
  if(plot_these$found){dev.off()}

# Contour plot ------------------------------------------------------------

  
  if(plot_these$contour){
    #options for contour plot
    plot_opmat <- t(opmat[grid_pos$rows:1,])#upside down and inside out
    contour_axes <- expression(
      { axis(1, at = (1:grid_pos$columns)/grid_pos$columns,
             labels = (1:grid_pos$columns)
      )
        axis(2, at = (1:grid_pos$rows)/grid_pos$rows,
             labels = rev(1:grid_pos$rows)
        )
      }
    )
    png(filename = file.path(img_path,'Out', paste0(ii, '-Contour.png')),
        height = 4, width = 5, units = 'in', bg = 'white', res = 150)
    filled.contour(plot_opmat,
                   main = ii,
                   nlevels = 20,
                   # col = contour_cols,
                   color.palette = function(n){rev(hcl.colors(n+1, cont_pal))},
                   zlim = c(0,1),
                   plot.axes = eval(contour_axes),
                   key.title = title(main = 'Colony density\n(per pixel)', cex.main = 0.5)
    )
    dev.off()
  }#if(plot_these$contour)
  
  
# Save data ---------------------------------------------------------
  write.csv(opmat, file = file.path(img_path,'Out',
                                    paste0(ii, '-OtsuQuantified.csv'))
  )
  #save all settings for each file
  write.table(t(data.frame(
                  time = Sys.time(),
                  image.name = ii,
                  folder = img_path,
                  grid_pos_ = grid_pos,
                  col_chan,
                  cont_pal,
                  saved.plots_ = plot_these,
                  crop_max,
                  edge_prop,
                  spc_lim,
                  im_levels,
                  R_version = R.version.string,
                  t(Sys.info())
                      )),
            file = file.path(img_path,'Out',
                                    paste0(ii, '-SettingsUsed.txt')),
            col.names = F,
            quote = F
  )
}#for(ii in imgs)
message('\n\n',
        '\n\t************************\n',
        '\t  All images processed',
        '\n\t************************\n',
        '\n\n'
        )
# . ---------------------------------------------------------------
# Discarded ---------------------------------------------------------------
# . ---------------------------------------------------------------
